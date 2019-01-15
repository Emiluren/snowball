extern crate tungstenite as ts;

mod entities;
mod vec2;
mod lobby;
mod level;
mod game;

use std::cell::RefCell;
use std::collections::HashMap;
use std::net::{TcpListener, TcpStream};
use std::sync::{Arc, Mutex, mpsc};
use std::thread;
use std::vec::Vec;
use tungstenite::handshake::server::Request;
use tungstenite::protocol::Role;
use crate::entities::Player;
use crate::lobby::{Lobby, Client};

fn parse_request(path: &str) -> Option<(String, String)> {
    let lobby_and_username: Vec<_> = path.split('/').filter(|s| !s.is_empty()).collect();

    match lobby_and_username.as_slice() {
        &[lobby, username] => Some((lobby.to_string(), username.to_string())),
        _ => None
    }
}

fn main() {
    let mut lobbies = HashMap::new();
    let server = TcpListener::bind("127.0.0.1:30000").unwrap();
    for stream_res in server.incoming() {
        let stream = stream_res.unwrap();

        // This is a bit tricky because the lobbies hash map is on the main
        // thread but the lobby websocket needs to be created on the lobby
        // thread. This is a problem because the lobby name is used as a key for
        // the hash map but this is not found out until after we create the
        // websocket.
        let (lobby_name_transmitter, lobby_name_receiver) = mpsc::channel();
        let (lobby_arc_transmitter, lobby_arc_receiver) = mpsc::channel();

        thread::spawn(move || {
            let mut con = init_connection(&stream);
            println!("{} has joined {}", &con.username, &con.lobby_name);

            lobby_name_transmitter.send(con.lobby_name.clone()).unwrap();
            let lobby_arc: Arc<Mutex<Lobby>> = lobby_arc_receiver.recv().unwrap();

            {
                let mut lobby = lobby_arc.lock().unwrap();
                let websocket2 = ts::WebSocket::from_raw_socket(stream, Role::Server);
                lobby.clients.insert(con.username.clone(), Client {
                    websocket: websocket2,
                    player: Player::new(con.username.clone()),
                });
            }

            loop {
                let message = con.websocket.read_message();

                match message {
                    Ok(msg) => {
                        handle_message(
                            &mut lobby_arc.lock().unwrap(),
                            msg,
                            &con.lobby_name,
                            &con.username,
                        );
                    },
                    _ => {
                        println!("{} disconnected from {}", &con.username, &con.lobby_name);
                        let deleted_last = {
                            let mut lobby = lobby_arc.lock().unwrap();
                            lobby.clients.remove(&con.username);
                            lobby.clients.is_empty()
                        };
                        if deleted_last {
                            // TODO: stop game thread if it is running
                            println!("The last person left {}. Removing that lobby, NOT IMPLEMENTED", &con.lobby_name);
                            // lobbies.remove(&con.lobby_name);
                        }
                        break;
                    }
                }
            }
        });

        let lobby_name = lobby_name_receiver.recv().unwrap();

        let mut lobby_arc = lobbies.entry(lobby_name).or_insert(
            Arc::new(Mutex::new(Lobby::new()))
        );

        lobby_arc_transmitter.send(lobby_arc.clone()).unwrap();
    }
}

struct Connection {
    lobby_name: String,
    username: String,
    websocket: ts::WebSocket<std::net::TcpStream>,
}

fn init_connection(stream: &std::net::TcpStream) -> Connection {
    let username = RefCell::new(String::new());
    let lobby_name = RefCell::new(String::new());
    let callback = |req: &Request| {
        match parse_request(&req.path) {
            Some((ln, un)) => {
                lobby_name.replace(ln);
                username.replace(un);
                Ok(None)
            }
            None => Err(ts::Error::Http(400))
        }
    };

    let mut websocket = ts::accept_hdr(
        stream.try_clone().unwrap(),
        callback
    ).unwrap();
    let lobby_name = lobby_name.borrow();
    let username = username.borrow();

    Connection {
        lobby_name: lobby_name.to_string(),
        username: username.to_string(),
        websocket: websocket,
    }
}

fn handle_message(
    lobby: &mut Lobby,
    msg: ts::protocol::Message,
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
            lobby.send_to_others(
                &username,
                &format!("chat:{}", &message_string),
            );
        },
        "start game" => {
            // TODO: start game

            // if thread.is_none() {
            //     let lobby = lobby.clone();
            //     thread = Some(thread::spawn(move || {
            //         game::run_main_loop(lobby);
            //     }));
            // }
        },
        _ => println!("Unknown message type: {}", message_text),
    }
}
