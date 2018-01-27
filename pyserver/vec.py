import math

def _from_direction(angle, length):
    return (math.cos(angle), math.sin(angle)) * length

def radians_to_degrees(angle):
    return (angle / math.pi) * 180

def add(this, other):
    this_x, this_y = this
    other_x, other_y = other
    return (this_x + other_x, this_y + other_y)

def sub(this, other):
    this_x, this_y = this
    other_x, other_y = other
    return (this_x - other_x, this_y - other_y)

def iadd(this, other):
    other_x, other_y = other
    this_x, this_y = this
    new_x = this_x + other_x
    new_y = this_y + other_y
    return (new_x, new_y)

def mul(this, other):
    this_x, this_y = this
    other_x, other_y = other
    return (this_x * other, this_y * other)

def length(this):
    this_x, this_y = this
    return math.sqrt(this_x ** 2 + this_y ** 2)

def is_within_bounds(this, center_point, size):
    """Checks whether this vector is within size distance from center_point"""
    return distance_to(this, center_point) <= size

def angle(this):
    this_x, this_y = this
    return math.atan2(this_y, this_x)

def relative_angle_to(this, other):
    return angle(sub(this, other))

def distance_to(this, other):
    return length(sub(this, other))

