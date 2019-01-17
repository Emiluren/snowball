use crate::lobby::{Lobby, Client};
use crate::entities::{Player, Snowball};
use std::collections::HashMap;
use std::collections::hash_map::Values;
use std::sync::{Arc, Mutex};
use crate::maploading;
use crate::level;
use rand::{self, Rng};
use crate::vec2::Vec2;
use std::vec;

const MAP_FILE: &str = "../public/assets/map.json";

const GRAVITY_ACCELERATION: f32 = 1.3;
const PLAYER_MAX_SPEED: f32 = 10.;
const SNOWBALL_DAMAGE: f32 = 0.5;

pub fn run_main_loop(lobby_arc: Arc<Mutex<Lobby>>) {
    let tile_map = maploading::load_tile_map(MAP_FILE);
    let mut lobby = lobby_arc.lock().unwrap();
    init_players(&mut lobby, &tile_map);

    let mut players: Vec<&mut Player> = lobby.clients.values_mut().map(|client| &mut client.player).collect();
    loop {
        // TODO update players
        //update_players(&players);
    }
}

fn init_players(lobby: &mut Lobby, tile_map: &maploading::Map) {
    for (_, client) in &mut lobby.clients {
        loop {
            let mut rng = rand::thread_rng();
            let rx_f: f32 = rng.gen();
            let ry_f: f32 = rng.gen();
            let rx: i32 = (rx_f*(tile_map.width as f32)) as i32;
            let ry: i32 = (ry_f*(tile_map.height as f32)) as i32;

            let collider = level::can_move_to(level::PLAYER_WIDTH,
                                              level::PLAYER_HEIGHT,
                                              rx, ry, tile_map, &None);

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
                                          tile_map, &None);
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
                                          tile_map, &None);
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


fn all_players(lobby: &Lobby) -> HashMap<String, &Player> {
    let mut result = HashMap::new();
    for (name, client) in &lobby.clients {
        result.insert(name.clone(), &client.player);
    }
    result
}


fn update_snowballs(lobby: &mut Lobby, tile_map: &maploading::Map) {
    let mut changed_healths: Vec<String> = Vec::new();
    let mut destroyed_snowballs: Vec<i32> = Vec::new();

    let mut player_hit = false;
    let mut ground_hit = false;
    for (id, snowball) in &mut lobby.snowballs {
        let new_vel = snowball.velocity + Vec2 {x: 0., y: GRAVITY_ACCELERATION};
        let new_pos = snowball.position + new_vel;
        snowball.velocity = snowball.velocity;
        snowball.position = snowball.position;
    }

    let mut hits = vec!();
    for (id, snowball) in &lobby.snowballs {
        let collider = level::can_move_to(level::PLAYER_WIDTH,
                                          level::PLAYER_HEIGHT,
                                          snowball.position.x.round() as i32,
                                          snowball.position.y.round() as i32,
                                          tile_map, &Some(all_players(lobby)));
        match collider {
            level::Collider::Player(name) => {
                destroyed_snowballs.push(*id);
                player_hit = true;
                hits.push((name, snowball.velocity.length()))
            },
            level::Collider::Tile(_, _) => {
                destroyed_snowballs.push(*id);
                ground_hit = true;
            },
            _ => { }
        }
    }

    for (player_name, speed) in hits {
        // NOTE: Safe unwrap because the player should exist if it was hit earlier
        let player = &mut lobby.clients.get_mut(&player_name).unwrap().player;
        player.health = (player.health as f32 - speed*SNOWBALL_DAMAGE).max(0.)
            as u32;
        changed_healths.push(player_name);
    }
    // TODO not done yet
}

