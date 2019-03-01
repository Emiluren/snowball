use std::ops::{Add, Mul, Sub};

#[derive(Debug, Copy, Clone)]
pub struct Vec2 {
    pub x: f32,
    pub y: f32
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

impl Mul<f32> for Vec2 {
    type Output = Vec2;

    fn mul(self, scalar: f32) -> Self {
        Vec2 {
            x: self.x*scalar,
            y: self.y*scalar,
        }
    }
}

impl Vec2 {
    pub fn from_direction(angle: f32, length: f32) -> Self {
        Vec2 {
            x: angle.cos() * length,
            y: angle.sin() * length,
        }
    }

    pub fn length(self) -> f32 {
        (self.x.powi(2) + self.y.powi(2)).sqrt()
    }

    pub fn angle(self) -> f32 {
        self.y.atan2(self.x)
    }

    pub fn distance_to(self, other: Self) -> f32 {
        (self - other).length()
    }

    pub fn relative_angle_to(self, other: Self) -> f32 {
        (self - other).angle()
    }

    pub fn is_within_bounds(self, center_point: Self, size: f32) -> bool {
        self.distance_to(center_point) <= size
    }
}
