extern crate simple_server;
extern crate ws;

use simple_server::Server;
use std::fs::File;
use std::collections::HashMap;
use std::io::Read;
use std::{sync, sync::{Arc, Mutex}};
use std::thread;
use std::vec::Vec;

struct Player {
    pub name: String,
    // TODO: add other fields
}

#[derive(Clone)]
struct Client {
    pub websocket: ws::Sender,
    pub lobbies: sync::Weak<Mutex<HashMap<String, Lobby>>>,
}

impl ws::Handler for Client {
    fn on_request(&mut self, req: &ws::Request) -> ws::Result<(ws::Response)> {
        let lobby_and_username: Vec<_> = req.resource().split('/').filter(|s| !s.is_empty()).collect();
        let (lobby_name, username) = match lobby_and_username.as_slice() {
            &[lobby, username] => (lobby, username),
            _ => {
                let mut res = ws::Response::from_request(req)?;
                res.set_status(400);
                res.set_reason("Expected /[lobby name]/[username]");
                return Ok(res);
            }
        };

        let lobbies_mutex = match self.lobbies.upgrade() {
            None => {
                let mut res = ws::Response::from_request(req)?;
                res.set_status(500);
                res.set_reason("Invalid server state");
                return Ok(res);
            }
            Some(lobbies) => {
                lobbies
            }
        };
        let mut lobbies = lobbies_mutex.lock().unwrap();

        let lobby = lobbies.entry(lobby_name.to_string()).or_insert(Lobby {
            clients: HashMap::new()
        });

        // TODO: check if game is already running here

        lobby.clients.insert(username.to_string(), self.clone());
        println!("{} has joined {}", username, lobby_name);
        ws::Response::from_request(req)
    }

    fn on_message(&mut self, msg: ws::Message) -> ws::Result<()> {
        self.websocket.send(msg).unwrap();
        Ok(())
    }
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

fn main() {
    thread::spawn(|| {
        serve_files();
    });

    let lobbies = Arc::new(Mutex::new(HashMap::new()));
    ws::listen("127.0.0.1:30000", |out| {
        out.send("Hello WebSocket").unwrap();
        Client {
            lobbies: Arc::downgrade(&lobbies),
            websocket: out,
        }
    }).unwrap();
}
