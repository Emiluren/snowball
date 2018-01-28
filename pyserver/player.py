
class Player:

    def __init__(self, name):
        self.name = name
        self.health = 0
        self.position = (10, 100)
        self.num_snowballs = 0
        self.left_pressed = False
        self.right_pressed = False
        self.velocity = (0, 0)
        self.on_ground = False


class Snowball:

    def __init__(self, id):
        self.id = id
        self.position = (0, 0)
        self.velocity = (0, 0)
