use crate::lobby::{Lobby, Client};
use crate::maploading;
use crate::level;
use rand::{self, Rng};


const MAP_FILE: &str = "../public/assets/map.json";


fn init_players(lobby: &mut Lobby, tile_map: maploading::Map) {
    for (name, client) in &lobby.clients {
        loop {
            let mut rng = rand::thread_rng();
            let rx_f: f32 = rng.gen();
            let ry_f: f32 = rng.gen();
            let rx: i32 = (rx_f*(tile_map.width as f32)) as i32;
            let ry: i32 = (ry_f*(tile_map.height as f32)) as i32;
            // TODO finish
        }
    }
}

pub fn run_main_loop(lobby: &mut Lobby) {
    let tile_map = maploading::load_tile_map(MAP_FILE);
}

