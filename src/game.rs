use crate::lobby::{Lobby, Client};
use crate::entities::{Player, Snowball};
use crate::maploading;
use crate::level;
use crate::vec2::Vec2;

use rand;
use rand::distributions::{Distribution, Uniform};

use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::sync::mpsc::Receiver;
use std::vec;
use std::thread::sleep;
use std::time::Duration;

const MAP_FILE: &str = "public/assets/map.json";

const GRAVITY_ACCELERATION: f32 = 1.3;
const PLAYER_MAX_SPEED: f32 = 10.;
const MAX_THROWING_FORCE: f32 = 30.;
const SNOWBALL_SPAWN_DISTANCE: f32 = 30.;
const SNOWBALL_DAMAGE: f32 = 0.5;

pub fn run_main_loop(lobby_arc: Arc<Mutex<Lobby>>, thread_killer: Receiver<()>) {
    let tile_map = maploading::load_tile_map(MAP_FILE);
    {
        let mut lobby = lobby_arc.lock().unwrap();
        init_players(&mut lobby, &tile_map);
    }

    loop {
        if let Ok(()) = thread_killer.try_recv() {
            println!("Stop signal received, shutting down game thread");
            break;
        }

        // Sleep before locking lobby so that the main thread has a chance to
        // access it
        sleep(Duration::from_millis(1000 / 30));

        let mut lobby = lobby_arc.lock().unwrap();
        update_players(&mut lobby.clients, &tile_map);
        let (changed_healths, destroyed_snowballs, player_hit, ground_hit) =
            update_snowballs(&mut lobby, &tile_map);

        // Broadcast changed healths
        for player in &changed_healths {
            let health = lobby.clients[player].player.health;
            lobby.broadcast(&format!("health:{} {}", player, health))
        }

        // Broadcasts destroyed snowballs
        for ball_id in &destroyed_snowballs {
            lobby.broadcast(&format!("delete ball:{}", ball_id))
        }

        // Broadcast snowball positions
        if !lobby.snowballs.is_empty() {
            // TODO: simulate snowballs on clients so they don't need to be
            // synced
            let mut message = "snowballs:".to_string();
            for snowball in lobby.snowballs.values() {
                message.push_str(&format!(
                    "{} {} {}",
                    snowball.id,
                    snowball.position.x,
                    snowball.position.y,
                ));
            }
            lobby.broadcast(&message);
        }

        if player_hit {
            // TODO: broadcast player hit audio
        }

        if ground_hit {
            // TODO: broadcast ground hit audio
        }

        // Broadcast player positions
        let player_data: Vec<_> =
            lobby.clients.values().map(
                |c| (
                    c.player.name.clone(),
                    c.player.position.x,
                    c.player.position.y,
                )
            ).collect();
        for (name, x, y) in player_data {
            lobby.broadcast(&format!("position:{} {} {}", name, x, y));
        }
    }
}

pub fn jump(player: &mut Player) {
    if player.on_ground {
        player.velocity = Vec2 {
            x: player.velocity.x,
            y: player.velocity.y - 20.
        };
    }
}

fn init_players(lobby: &mut Lobby, tile_map: &maploading::Map) {
    for (_, client) in &mut lobby.clients {
        loop {
            let x_dist = Uniform::from(0..tile_map.width);
            let y_dist = Uniform::from(0..tile_map.height);
            let mut rng = rand::thread_rng();
            let rx = x_dist.sample(&mut rng) as i32;
            let ry = y_dist.sample(&mut rng) as i32;

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

fn update_players(clients: &mut HashMap<String, Client>, tile_map: &maploading::Map) {
    for client in clients.values_mut()  {
        update_player(&mut client.player, tile_map);
    }
}

fn all_players(lobby: &Lobby) -> HashMap<String, &Player> {
    let mut result = HashMap::new();
    for (name, client) in &lobby.clients {
        result.insert(name.clone(), &client.player);
    }
    result
}

fn update_snowballs(lobby: &mut Lobby, tile_map: &maploading::Map) -> (Vec<String>, Vec<usize>, bool, bool) {
    let mut changed_healths = Vec::new();
    let mut destroyed_snowballs = Vec::new();

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
            _ => {}
        }
    }

    for (player_name, speed) in hits {
        // NOTE: Safe unwrap because the player should exist if it was hit earlier
        let player = &mut lobby.clients.get_mut(&player_name).unwrap().player;
        player.health = (player.health as f32 - speed*SNOWBALL_DAMAGE).max(0.)
            as u32;
        changed_healths.push(player_name);
    }

    for id in &destroyed_snowballs {
        lobby.snowballs.remove(&id);
    }

    (changed_healths, destroyed_snowballs, player_hit, ground_hit)
}

fn create_snowball_id(lobby: &Lobby) -> usize {
    match lobby.snowballs.keys().max() {
        None => 0,
        Some(id) => id + 1,
    }
}

pub fn create_snowball(lobby: &mut Lobby, pos: Vec2, angle: f32, force: f32) {
    let direction = Vec2::from_direction(angle, 1.);
    let snowball = Snowball {
        id: create_snowball_id(lobby),
        position: pos + direction.scale(SNOWBALL_SPAWN_DISTANCE),
        velocity: direction.scale(force * MAX_THROWING_FORCE),
    };
    lobby.snowballs.insert(snowball.id, snowball);
}
