use crate::lobby::{Lobby, Client};
use crate::entities::Player;
use std::collections::HashMap;
use crate::maploading;

pub const TILE_SIZE: usize = 20;

pub enum Collider {
    Player(Player),
    Tile(i32, i32),
    ScreenBorder,
    NoCollision,
}


pub fn is_outside_screen(width: usize, height: usize, new_x: i32, new_y: i32,
                         tile_map: &maploading::Map) -> bool {
    new_x < 0 ||
    (new_x as usize) + width >= tile_map.width*TILE_SIZE ||
    (new_y as usize) + height >= tile_map.height*TILE_SIZE
}


pub fn can_move_to(width: usize, height: usize, new_x: i32, new_y: i32,
                   tile_map: &maploading::Map,
                   lobby: Option<&Lobby>) -> Collider {
    if is_outside_screen(width, height, new_x, new_y, tile_map) {
        return Collider::ScreenBorder;
    }

    if new_y >= 0 {
        let left: i32 = new_x / (TILE_SIZE as i32);
        let right: i32 = (new_x + (width as i32)) / (TILE_SIZE as i32);
        let top: i32 = new_y / (TILE_SIZE as i32);
        let bottom: i32 = (new_y + (height as i32)) / (TILE_SIZE as i32);

        for y in top..bottom+1 {
            for x in left..right+1 {
                if !tile_map.is_transparent(x as usize, y as usize) {
                    return Collider::Tile(x*(TILE_SIZE as i32), 
                                          y*(TILE_SIZE as i32));
                }
            }
        }
    }
    // TODO this doesnt compile
    // match lobby {
    //     Some (lobby) => {
    //         for (name, client) in lobby.clients {
    //             let pos = client.player.position;
    //             return Collider::Player(client.player)
    //         }
    //         Collider::NoCollision
    //     }
    //     None => Collider::NoCollision
    // }
    Collider::NoCollision
}

