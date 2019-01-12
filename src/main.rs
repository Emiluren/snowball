extern crate tungstenite as ts;

mod entities;
mod vec2;

use std::cell::RefCell;
use std::collections::HashMap;
use std::net::{TcpListener, TcpStream};
use std::sync::{Arc, Mutex};
use std::thread;
use std::vec::Vec;
use ts::handshake::server::Request;
use ts::protocol::Role;
use entities::Player;


#[derive(Debug)]
struct Lobby {
    pub clients: HashMap<String, Client>,
    // TODO: add snowballs
    // TODO: add thread
}

impl Lobby {
    pub fn send_to_others(&mut self, username: &str, message: &str) {
        let message = ts::Message::Text(message.to_string());
        for (client_name, client) in &mut self.clients {
            if client_name != username {
                client.websocket.write_message(message.clone()).unwrap();
            }
        }
    }
}

struct Client {
    pub websocket: ts::WebSocket<TcpStream>,
    pub player: Player,
}

impl std::fmt::Debug for Client {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("Client").field("player", &self.player).finish()
    }
}

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
        clients: HashMap::new()
    });

    // TODO: check if game is already running here

    lobby.clients.insert(username.to_string(), Client {
        websocket: websocket,
        player: Player::new(username.to_string()),
    });
}

fn main() {
    let lobbies = Arc::new(Mutex::new(HashMap::new()));
    let server = TcpListener::bind("127.0.0.1:30000").unwrap();
    for stream_res in server.incoming() {
        let lobbies = lobbies.clone();
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
                lobbies.clone(),
                websocket2,
                &lobby_name,
                &username,
            );
            println!("{} has joined {}", username, lobby_name);

            loop {
                let message = websocket.read_message();
                let mut lobbies = lobbies.lock().unwrap();

                match message {
                    Ok(msg) => {
                        if msg.is_text() {
                            let message_text = msg.into_text().unwrap();
                            let split_message: Vec<_> =
                                message_text.splitn(2, ':').collect();
                            let message_type = split_message[0];
                            let message_content = split_message.get(1).unwrap_or(&"");
                            // TODO: break this out somehow maybe
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
