use crate::lobby::{Lobby, Client};
use crate::entities::Player;
use std::collections::HashMap;


//struct JSONMap {
//    pub layers: 
//}


pub enum Collider {
    Player(Player),
    Tile(i32, i32),
    ScreenBorder,
    NoCollision,
}


pub fn load_level() {

}


//pub fn can_move_to(width: u32, height: u32, new_x: f32, new_y: f32,
//                   lobby: Option<Lobby>) -> Collider {
//    
//}

