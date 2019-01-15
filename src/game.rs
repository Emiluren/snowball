use crate::lobby::{Lobby, Client};
use crate::entities::{Player, Snowball};
use std::collections::HashMap;
use std::collections::hash_map::Values;
use crate::maploading;
use crate::level;
use rand::{self, Rng};
use crate::vec2::Vec2;

const MAP_FILE: &str = "../public/assets/map.json";

const GRAVITY_ACCELERATION: f32 = 1.3;
const PLAYER_MAX_SPEED: f32 = 10.;

pub fn run_main_loop(lobby: &mut Lobby) {
    let tile_map = maploading::load_tile_map(MAP_FILE);
    init_players(lobby, &tile_map);

    let mut players: Vec<&mut Player> = lobby.clients.values_mut().map(|client| &mut client.player).collect();
    loop {
        // TODO update players
        //update_players(&players);
    }
}

fn init_players(lobby: &mut Lobby, tile_map: &maploading::Map) {
    for (name, client) in &mut lobby.clients {
        loop {
            let mut rng = rand::thread_rng();
            let rx_f: f32 = rng.gen();
            let ry_f: f32 = rng.gen();
            let rx: i32 = (rx_f*(tile_map.width as f32)) as i32;
            let ry: i32 = (ry_f*(tile_map.height as f32)) as i32;

            let collider = level::can_move_to(level::PLAYER_WIDTH,
                                              level::PLAYER_HEIGHT,
                                              rx, ry, tile_map, None);

            match collider {
                level::Collider::NoCollision => {
                    client.player.position = Vec2 {x:rx as f32, y:ry as f32};
                    break;
                },
                _ => { }
            }
        }
    }
}


fn update_player(player: &mut Player, tile_map: &maploading::Map) {
    // feel free to un-shittify this shitty translated python code

    // y-direction
    {
        let pos = player.position;
        let vel = player.velocity;

        let new_pos_y = (pos.y + vel.y).round();
        let collider = level::can_move_to(level::PLAYER_WIDTH,
                                          level::PLAYER_HEIGHT,
                                          pos.x as i32, new_pos_y as i32,
                                          tile_map, None);
        match collider {
            
            level::Collider::NoCollision => {
                player.velocity = Vec2{x: vel.x, y: vel.y + GRAVITY_ACCELERATION};
                player.position = Vec2{x: pos.x, y: new_pos_y};
                if pos.y != player.position.y {
                    player.on_ground = false;
                }
            },
            level::Collider::Tile(tx, ty) => {
                if vel.y > 0. {
                    player.position = Vec2{x: pos.x,
                                           y: ty as f32 - level::PLAYER_HEIGHT as f32};
                }
                player.velocity = Vec2{x: vel.x, y: 0.};
                player.on_ground = true;
            },
            _ => { }
        }
    }

    // x-direction
    {
        let pos = player.position;
        let vel = player.velocity;

        let new_pos_x = (pos.x + vel.x).round();
        let collider = level::can_move_to(level::PLAYER_WIDTH,
                                          level::PLAYER_HEIGHT,
                                          new_pos_x as i32, pos.y as i32,
                                          tile_map, None);
        match collider {
            
            level::Collider::NoCollision => {
                let dx = -(player.left_pressed as i32) + player.right_pressed as i32;
                let mut new_vx = vel.x * 0.9 + dx as f32;
                if new_vx > PLAYER_MAX_SPEED {
                    new_vx = PLAYER_MAX_SPEED
                }
                player.velocity = Vec2{x: new_vx, y: vel.y};
                player.position = Vec2{x: (pos.x + vel.x).round(), y: pos.y};
            },
            level::Collider::Tile(tx, ty) => {
                player.velocity = Vec2{x: 0., y: vel.y}
            },
            _ => { }
        }
    }
}


fn update_players(players: Vec<&mut Player>, tile_map: &maploading::Map) {
    for player in players {
        update_player(player, tile_map);
    }
}

