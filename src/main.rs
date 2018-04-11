extern crate simple_server;

use simple_server::Server;
use std::fs::File;
use std::io::Read;
use std::vec::Vec;

fn main() {
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
