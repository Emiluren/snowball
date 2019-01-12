
use std::ops::Add;
use std::ops::Sub;
use std::ops::Mul;

#[derive(Debug)]
pub struct Vec2 {
    pub x: f64,
    pub y: f64
}

impl Add for Vec2 {
    type Output = Vec2;

    fn add(self, other: Vec2) -> Self {
        Vec2 {
            x: self.x + other.x,
            y: self.y + other.y
        }
    }
}


impl Sub for Vec2 {
    type Output = Vec2;

    fn sub(self, other: Vec2) -> Self {
        Vec2 {
            x: self.x - other.x,
            y: self.y - other.y
        }
    }
}

impl Vec2 {
    pub fn scale(self, scalar: f64) -> Self {
        Vec2 {
            x: self.x*scalar,
            y: self.y*scalar,
        }
    }
}



