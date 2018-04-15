extern crate tungstenite as ts;

use std::cell::RefCell;
use std::collections::HashMap;
use std::net::{TcpListener, TcpStream};
use std::sync::{Arc, Mutex};
use std::thread;
use std::vec::Vec;
use ts::handshake::server::Request;
use ts::protocol::Role;

struct Player {
    pub name: String,
    // TODO: add other fields
}

struct Lobby {
    pub clients: HashMap<String, Client>,
    // TODO: add snowballs
    // TODO: add thread
}

struct Client {
    pub websocket: ts::WebSocket<TcpStream>,
    pub player: Player,
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
    lobby_name: &String,
    username: &String,
) {
    let mut lobbies = lobbies.lock().unwrap();

    let lobby = lobbies.entry(lobby_name.to_string()).or_insert(Lobby {
        clients: HashMap::new()
    });

    // TODO: check if game is already running here

    lobby.clients.insert(username.to_string(), Client {
        websocket: websocket,
        player: Player {
            name: username.to_string(),
        },
    });
}

fn main() {
    let lobbies = Arc::new(Mutex::new(HashMap::new()));
    let server = TcpListener::bind("127.0.0.1:30000").unwrap();
    for stream_res in server.incoming() {
        let lobbies_clone = lobbies.clone();
        thread::spawn(move || {
            let username = RefCell::new(String::new());
            let lobby_name = RefCell::new(String::new());
            let callback = |req: &Request| {
                match parse_request(&req.path) {
                    Some((un, ln)) => {
                        username.replace(un);
                        lobby_name.replace(ln);
                        Ok(None)
                    }
                    None => Err(ts::Error::Http(400))
                }
            };

            let stream = stream_res.unwrap();
            let mut websocket = ts::accept_hdr(stream.try_clone().unwrap(), callback).unwrap();
            let websocket2 = ts::WebSocket::from_raw_socket(stream, Role::Server);
            add_new_client_to_lobby(
                lobbies_clone, websocket2, &lobby_name.borrow(), &username.borrow());
            println!("{} has joined {}", username.borrow(), lobby_name.borrow());

            loop {
                match websocket.read_message() {
                    Ok(msg) => {
                        if msg.is_binary() || msg.is_text() {
                            websocket.write_message(msg).unwrap();
                        }
                    },
                    _ => {
                        // TODO: could not read, remove player from lobby
                    }
                }
            }
        });
    }
}
