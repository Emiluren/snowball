extern crate hyper;
extern crate simple_server;
extern crate websocket as ws;

use hyper::uri::RequestUri;
use simple_server::Server;
use std::fs::File;
use std::collections::HashMap;
use std::io::Read;
use std::{sync, sync::{Arc, Mutex}};
use std::thread;
use std::vec::Vec;
use ws::client::sync::Client as WsClient;
use ws::sync::stream::{TcpStream};

struct Player {
    pub name: String,
    // TODO: add other fields
}

//    pub lobbies: sync::Weak<Mutex<HashMap<String, Lobby>>>,
struct Client {
    pub websocket: WsClient<TcpStream>,
    pub player: Player,
}

struct Lobby {
    pub clients: HashMap<String, Client>,
}

fn serve_files() {
    let server = Server::new(|_request, mut response| {
        // Redirect to index.html for all requests that don't already specify
        // a static file
        let mut f = File::open("public/index.html")?;
        let mut source = Vec::new();
        f.read_to_end(&mut source)?;
        Ok(response.body(source)?)
    });
    server.listen("127.0.0.1", "7979");
}

fn parse_lobby_and_username(request_uri: RequestUri) -> Option<(String, String)>
{
    let path = match request_uri {
        RequestUri::AbsolutePath(p) => p,
        _ => {
            return None;
        },
    };

    let lobby_and_username: Vec<_> =
        path.split('/').filter(|s| !s.is_empty()).collect();

    match lobby_and_username.as_slice() {
        &[lobby, username] => Some((lobby.to_string(), username.to_string())),
        _ => None
    }
}

fn main() {
    thread::spawn(|| {
        serve_files();
    });

    let lobbies_mutex = Arc::new(Mutex::new(HashMap::new()));
    let server = ws::sync::Server::bind("127.0.0.1:30000").unwrap();

    for ws_upgrade in server.filter_map(Result::ok) {
        let lobbies_ref = lobbies_mutex.clone();
        let path = ws_upgrade.request.subject.1.clone();

        thread::spawn(move || {
            match parse_lobby_and_username(path) {
                None => {
                    ws_upgrade.reject().unwrap();
                    return;
                },
                Some((lobby_name, username)) => {
                    let mut lobbies = lobbies_ref.lock().unwrap();

                    let lobby = lobbies.entry(lobby_name.to_string()).or_insert(Lobby {
                        clients: HashMap::new()
                    });

                    // TODO: check if game is already running here

                    lobby.clients.insert(username.to_string(), Client {
                        websocket: ws_upgrade.accept().unwrap(),
                        player: Player {
                            name: username.to_string(),
                        }
                    });
                    println!("{} has joined {}", username, lobby_name);
                }
            }
        });
    }
}
