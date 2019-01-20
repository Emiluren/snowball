use crate::entities::Player;
use crate::vec2::*;
use std::collections::HashMap;
use crate::maploading;

pub const TILE_SIZE: usize = 20;
pub const PLAYER_WIDTH: usize = 40;
pub const PLAYER_HEIGHT: usize = 60;

pub enum Collider {
    Player(String),
    Tile(i32, i32),
    ScreenBorder,
    NoCollision,
}

pub fn is_outside_screen(width: usize, height: usize, new_x: i32, new_y: i32,
                         tile_map: &maploading::Map) -> bool {
    new_x < 0 || new_y < 0 ||
    (new_x as usize) + width >= tile_map.width*TILE_SIZE ||
    (new_y as usize) + height >= tile_map.height*TILE_SIZE
}

pub fn can_move_to(width: usize, height: usize, new_x: i32, new_y: i32,
                   tile_map: &maploading::Map,
                   players: &Option<HashMap<String, &Player>>) -> Collider {
    if is_outside_screen(width, height, new_x, new_y, tile_map) {
        return Collider::ScreenBorder;
    }

    if new_y >= 0 {
        let left = new_x / (TILE_SIZE as i32);
        let right = (new_x + (width as i32)) / (TILE_SIZE as i32);
        let top = new_y / (TILE_SIZE as i32);
        let bottom = (new_y + (height as i32)) / (TILE_SIZE as i32);

        for y in top..=bottom {
            for x in left..=right {
                if !tile_map.is_transparent(x as usize, y as usize) {
                    return Collider::Tile(x*(TILE_SIZE as i32), 
                                          y*(TILE_SIZE as i32));
                }
            }
        }
    }

    if let Some(players) = players {
        for (name, player) in players {
            let pos: Vec2 = player.position;
            if overlaps(new_x,
                        new_x + width as i32, 
                        pos.x as i32,
                        pos.x as i32 + PLAYER_WIDTH as i32)
                &&
                overlaps(new_y,
                         new_y + height as i32,
                         pos.y as i32,
                         pos.y as i32 + PLAYER_HEIGHT as i32) {
                    return Collider::Player(name.to_string());
                }
        }
    }
    return Collider::NoCollision
}

fn overlaps(a1: i32, a2: i32, b1: i32, b2: i32) -> bool {
    a1.max(b1) <= b2.min(a2)
}
