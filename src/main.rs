extern crate ws;

use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::thread;
use std::vec::Vec;

struct Player {
    pub name: String,
    // TODO: add other fields
}

struct Lobby {
    pub clients: HashMap<String, Client>,
}

struct Client {
}

struct ClientHandler {
    pub websocket: ws::Sender,
    pub lobbies: Arc<Mutex<HashMap<String, Lobby>>>,
    pub lobby_name: String,
    pub username: String,
}

impl ws::Handler for ClientHandler {
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

        self.lobby_name = lobby_name.to_string();
        self.username = username.to_string();

        let mut lobbies = self.lobbies.lock().unwrap();

        let lobby = lobbies.entry(lobby_name.to_string()).or_insert(Lobby {
            clients: HashMap::new()
        });

        // TODO: check if game is already running here

        lobby.clients.insert(username.to_string(), Client {
        });
        println!("{} has joined {}", username, lobby_name);
        ws::Response::from_request(req)
    }

    fn on_message(&mut self, msg: ws::Message) -> ws::Result<()> {
        self.websocket.send(msg).unwrap();
        Ok(())
    }
}

fn main() {
    let lobbies = Arc::new(Mutex::new(HashMap::new()));
    ws::listen("127.0.0.1:30000", |out| {
        out.send("chat:Hello WebSocket").unwrap();
        ClientHandler {
            lobbies: lobbies.clone(),
            websocket: out,
            lobby_name: String::new(),
            username: String::new(),
        }
    }).unwrap();
}
