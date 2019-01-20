use serde_derive::Deserialize;
use serde_json;
use std::fs::File;
use std::io::BufReader;

#[derive(Deserialize, Clone)]
pub struct Map {
    pub data: Vec<i32>,
    pub height: usize,
    pub width: usize,
}

pub fn load_tile_map(filename: &str) -> Map {
    let file = File::open(filename).unwrap();
    let reader = BufReader::new(file);
    let wrapped_map: JSONMap = serde_json::from_reader(reader).unwrap();
    wrapped_map.layers[0].clone()
}

#[derive(Deserialize)]
struct JSONMap {
    pub layers: Vec<Map>,
}

impl Map {
    pub fn get_cell(&self, x: usize, y: usize) -> i32 {
        self.data[x + y*self.width]
    }

    pub fn is_transparent(&self, x: usize, y: usize) -> bool {
        self.get_cell(x, y) == 0
    }
}
