extern crate actix;
extern crate actix_web;

use actix::{Actor, StreamHandler};
use actix_web::{http, server, ws, App, HttpRequest, HttpResponse};
use std::collections::HashMap;
use std::fs::File;
use std::io::Read;
use std::sync::{Arc, Mutex};
use std::thread;
use std::vec::Vec;

struct Player {
    pub name: String,
    // TODO: add other fields
}

//    pub lobbies: sync::Weak<Mutex<HashMap<String, Lobby>>>,
// struct Client<S: Stream> {
//     pub websocket: WsClient<S>,
//     pub player: Player,
// }

// struct Lobby<S: Stream> {
//     pub clients: HashMap<String, Client<S>>,
// }

// fn serve_files() {
//     let server = Server::new(|_request, mut response| {
//         // Redirect to index.html for all requests that don't already specify
//         // a static file
//         let mut f = File::open("public/index.html")?;
//         let mut source = Vec::new();
//         f.read_to_end(&mut source)?;
//         Ok(response.body(source)?)
//     });
//     server.listen("127.0.0.1", "7979");
// }

// fn parse_lobby_and_username(request_uri: RequestUri) -> Option<(String, String)>
// {
//     let path = match request_uri {
//         RequestUri::AbsolutePath(p) => p,
//         _ => {
//             return None;
//         },
//     };

//     let lobby_and_username: Vec<_> =
//         path.split('/').filter(|s| !s.is_empty()).collect();

//     match lobby_and_username.as_slice() {
//         &[lobby, username] => Some((lobby.to_string(), username.to_string())),
//         _ => None
//     }
// }

// fn handle_upgrade_request<S>(
//     lobbies_mutex: Arc<Mutex<HashMap<String, Lobby<S>>>>,
//     upgrade: &Upgrade<S>,
// ) where S: Stream {
//     let path = upgrade.request.subject.1.clone();
    
//     match parse_lobby_and_username(path) {
//         None => {
//             upgrade.reject().ok().expect("Could not reject upgrade");
//             return;
//         },
//         Some((lobby_name, username)) => {
//             let mut lobbies = lobbies_mutex.lock().unwrap();
//                 let lobby = lobbies.entry(lobby_name.to_string()).or_insert(Lobby {
//                     clients: HashMap::new()
//                 });

//             let ws_client = upgrade.accept().ok().expect("Could not accept request");
//             lobby.clients.insert(username.to_string(), Client {
//                 websocket: ws_client,
//                 player: Player {
//                     name: username.to_string(),
//                 }
//             });
//             println!("{} has joined {}", username, lobby_name);
//         }
//     };

//     // TODO: check if game is already running here

//     thread::spawn(move || {
//     });
// }

struct ClientHandler {
    pub lobby_name: String,
    pub username: String,
}

impl ClientHandler {
    fn new(lobby_name: &str, username: &str) -> Self {
        ClientHandler {
            lobby_name: lobby_name.to_string(),
            username: username.to_string(),
        }
    }
}

impl Actor for ClientHandler {
    type Context = ws::WebsocketContext<Self>;
}

impl StreamHandler<ws::Message, ws::ProtocolError> for ClientHandler {
    fn handle(&mut self, msg: ws::Message, ctx: &mut Self::Context) {
        match msg {
            ws::Message::Ping(msg) => ctx.pong(&msg),
            ws::Message::Text(text) => ctx.text(text),
            ws::Message::Binary(bin) => ctx.binary(bin),
            _ => (),
        }
    }
}

fn main() {
    server::new(|| {
        let mut index_file = File::open("public/index.html").unwrap();
        let mut index_source = Vec::new();
        index_file.read_to_end(&mut index_source).unwrap();

        App::new()
            .route("/", http::Method::GET, move |_: HttpRequest|
                   HttpResponse::Ok().body(index_source.clone()))
            .resource("/ws/{lobby_name}/{username}", |r| r.f(|req| {
                let req_clone = req.clone();
                let params = req_clone.match_info();
                let lobby_name = params.get("lobby_name").unwrap();
                let username = params.get("username").unwrap();
                ws::start(req, ClientHandler::new(lobby_name, username))
            }))
            .handler(
                "/",
                actix_web::fs::StaticFiles::new("public/"))
    })
        .bind("127.0.0.1:8080").expect("Can not bind to 127.0.0.1:8088")
        .run();
}
