use vec2::Vec2;

#[derive(Debug)]
pub struct Player {
    pub name: String,
    pub health: u32,
    pub position: Vec2,
    pub num_snowballs: u32,
    pub left_pressed: bool,
    pub right_pressed: bool,
    pub velocity: Vec2,
    pub on_ground: bool,
}

#[derive(Debug)]
pub struct Snowball {
    pub id: i32,
    pub position: Vec2,
    pub velocity: Vec2
}

impl Player {
    pub fn new(name: String) -> Self {
        Self {
            name: name,
            health: 100,
            position: Vec2 {x:10.0, y:100.0},
            num_snowballs: 0,
            left_pressed: false,
            right_pressed: false,
            velocity: Vec2 {x:0., y:0.},
            on_ground: false,
        }
    }
}

