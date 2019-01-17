mod entities;
mod vec2;
mod lobby;
mod level;
mod game;
mod maploading;

use std::cell::RefCell;
use std::collections::HashMap;
use std::net::TcpListener;
use std::sync::{Arc, Mutex, mpsc::channel};
use std::thread;
use std::vec::Vec;
use tungstenite::{accept_hdr, WebSocket};
use tungstenite::handshake::server::Request;
use tungstenite::Message;
use tungstenite::protocol::Role;
use crate::entities::Player;
use crate::lobby::{Lobby, Client};

fn main() {
    let mut lobbies = HashMap::new();
    let server = TcpListener::bind("127.0.0.1:30000").unwrap();
    let (lobby_closer_transmitter, lobby_closer_receiver) = channel();

    for stream_res in server.incoming() {
        let stream = stream_res.unwrap();
        let lobby_closer_transmitter = lobby_closer_transmitter.clone();

        // This is a bit tricky because the lobbies hash map is on the main
        // thread but the lobby websocket needs to be created on the lobby
        // thread. This is a problem because the lobby name is used as a key for
        // the hash map but this is not found out until after we create the
        // websocket.
        let (lobby_name_transmitter, lobby_name_receiver) = channel();
        let (lobby_arc_transmitter, lobby_arc_receiver) = channel();

        // TODO: maybe move closure to a function
        thread::spawn(move || {
            let mut con = init_connection(&stream);
            println!("{} has joined {}", &con.username, &con.lobby_name);

            lobby_name_transmitter.send(con.lobby_name.clone()).unwrap();
            let lobby_arc: Arc<Mutex<Lobby>> = lobby_arc_receiver.recv().unwrap();

            let player_add_successful =
                add_player_to_lobby(lobby_arc.clone(), &con, stream);

            if player_add_successful {
                loop {
                    let message = con.websocket.read_message();

                    match message {
                        Ok(msg) => {
                            handle_message(
                                lobby_arc.clone(),
                                msg,
                                &con.lobby_name,
                                &con.username,
                            );
                        },
                        _ => {
                            // The player disconnected, remove them
                            println!("{} disconnected from {}", &con.username, &con.lobby_name);
                            let mut lobby = lobby_arc.lock().unwrap();
                            lobby.clients.remove(&con.username);

                            // Stop game thread if it's running
                            if let Some(thread_killer) = &lobby.game_thread {
                                thread_killer.send(()).unwrap();
                            }

                            // Remove the lobby if the last player left
                            if lobby.clients.is_empty() {
                                println!("The last person left {}. Removing that lobby", &con.lobby_name);
                                lobby_closer_transmitter.send(con.lobby_name.clone()).unwrap();
                            }
                            break;
                        }
                    }
                }
            }
        });

        // Check for any lobbies to remove
        while let Ok(lobby_to_close) = lobby_closer_receiver.try_recv() {
            lobbies.remove(&lobby_to_close);
        }

        // Find the requested lobby or create it if it does not already exist
        let lobby_name = lobby_name_receiver.recv().unwrap();
        let lobby_arc = lobbies.entry(lobby_name).or_insert(
            Arc::new(Mutex::new(Lobby::new()))
        );
        lobby_arc_transmitter.send(lobby_arc.clone()).unwrap();
    }
}

struct Connection {
    lobby_name: String,
    username: String,
    websocket: WebSocket<std::net::TcpStream>,
}

fn init_connection(stream: &std::net::TcpStream) -> Connection {
    let username = RefCell::new(String::new());
    let lobby_name = RefCell::new(String::new());

    let websocket = accept_hdr(
        stream.try_clone().unwrap(),
        |req: &Request| {
            let lobby_and_username: Vec<_> = req.path.split('/').filter(|s| !s.is_empty()).collect();

            match lobby_and_username.as_slice() {
                &[lobby, user] => {
                    lobby_name.replace(lobby.to_string());
                    username.replace(user.to_string());
                    Ok(None)
                }
                _ => Err(tungstenite::Error::Http(400))
            }
        }
    ).unwrap();
    let lobby_name = lobby_name.borrow();
    let username = username.borrow();

    Connection {
        lobby_name: lobby_name.to_string(),
        username: username.to_string(),
        websocket: websocket,
    }
}

fn add_player_to_lobby(lobby_arc: Arc<Mutex<Lobby>>, con: &Connection, stream: std::net::TcpStream) -> bool {
    let mut lobby = lobby_arc.lock().unwrap();
    let mut out_going_socket = WebSocket::from_raw_socket(stream, Role::Server, None);

    if lobby.game_thread.is_some() {
        // Game is already running, tell player to find another lobby
        println!("{} is already playing!", &con.lobby_name);
        let msg = format!(
            "chat:(Server) This lobby ({}) is already playing a game. {} cannot join now.",
            &con.lobby_name,
            &con.username
        );
        out_going_socket.write_message(Message::Text(msg)).unwrap();
        return false;
    }

    lobby.clients.insert(con.username.clone(), Client {
        websocket: out_going_socket,
        player: Player::new(con.username.clone()),
    });
    true
}


fn handle_message(
    lobby_arc: Arc<Mutex<Lobby>>,
    msg: tungstenite::protocol::Message,
    lobby_name: &str,
    username: &str,
) {
    if !msg.is_text() {
        return;
    }

    let message_text = msg.into_text().unwrap();
    let split_message: Vec<_> =
        message_text.splitn(2, ':').collect();
    let message_type = split_message[0];
    let message_content = split_message.get(1).unwrap_or(&"");

    match message_type {
        "chat" => {
            let message_string = format!(
                "({}) {}: {}", lobby_name, username, message_content
            );
            println!("{}", message_string);
            let mut lobby = lobby_arc.lock().unwrap();
            lobby.send_to_others(
                &username,
                &format!("chat:{}", &message_string),
            );
        },
        "start game" => {
            let lobby_arc_clone = lobby_arc.clone();
            let mut lobby = lobby_arc.lock().unwrap();
            if lobby.game_thread.is_none() {
                let (thread_kill_sender, thread_kill_receiver) = channel();
                thread::spawn(move || {
                    game::run_main_loop(lobby_arc_clone, thread_kill_receiver);
                });

                lobby.game_thread = Some(thread_kill_sender);
            }
        },
        _ => println!("Unknown message type: {}", message_text),
    }
}
