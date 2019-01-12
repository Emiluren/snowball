extern crate tungstenite as ts;

mod entities;
mod vec2;
mod lobby;

use std::cell::RefCell;
use std::collections::HashMap;
use std::net::{TcpListener, TcpStream};
use std::sync::{Arc, Mutex};
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

fn add_new_client_to_lobby(
    lobbies: Arc<Mutex<HashMap<String, Lobby>>>,
    websocket: ts::WebSocket<TcpStream>,
    lobby_name: &str,
    username: &str,
) {
    let mut lobbies = lobbies.lock().unwrap();

    let lobby = lobbies.entry(lobby_name.to_string()).or_insert(Lobby {
        clients: HashMap::new(),
        snowballs: HashMap::new(),
    });

    // TODO: check if game is already running here

    lobby.clients.insert(username.to_string(), Client {
        websocket: websocket,
        player: Player::new(username.to_string()),
    });
}

fn main() {
    let lobbies_arc = Arc::new(Mutex::new(HashMap::new()));
    let server = TcpListener::bind("127.0.0.1:30000").unwrap();
    for stream_res in server.incoming() {
        let lobbies_arc = lobbies_arc.clone();
        thread::spawn(move || {
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

            let stream = stream_res.unwrap();
            let mut websocket = ts::accept_hdr(
                stream.try_clone().unwrap(),
                callback
            ).unwrap();
            let websocket2 = ts::WebSocket::from_raw_socket(stream, Role::Server);
            let lobby_name = lobby_name.borrow();
            let username = username.borrow();
            add_new_client_to_lobby(
                lobbies_arc.clone(),
                websocket2,
                &lobby_name,
                &username,
            );
            println!("{} has joined {}", username, lobby_name);

            loop {
                let message = websocket.read_message();
                let mut lobbies = lobbies_arc.lock().unwrap();

                match message {
                    Ok(msg) => {
                        handle_message(
                            &mut lobbies,
                            msg,
                            &lobby_name,
                            &username,
                        );
                    },
                    _ => {
                        println!("{} disconnected from {}", username, lobby_name);
                        let deleted_last = {
                            let mut lobby = lobbies.get_mut(&*lobby_name).expect(
                                &format!("Could not access lobby {}", lobby_name)
                            );
                            lobby.clients.remove(&*username);
                            lobby.clients.is_empty()
                        };
                        if deleted_last {
                            // TODO: stop game thread if it is running
                            println!("The last person left {}. Removing that lobby", lobby_name);
                            lobbies.remove(&*lobby_name);
                        }
                        break;
                    }
                }
            }
        });
    }
}

fn handle_message(
    lobbies: &mut HashMap<String, Lobby>,
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

    let mut lobby = lobbies.get_mut(&*lobby_name).unwrap();

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
        _ => println!("Unknown message type: {}", message_text),
    }
}
