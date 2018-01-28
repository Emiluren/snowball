import json
import pdb

JSON_MAP = '../public/assets/map.json'

TILE_SIZE = 20
PLAYER_WIDTH = 40
PLAYER_HEIGHT = 60

tiles = None
players = {}

def print_tiles():
    s = ''
    for row in tiles:
        for t in row:
            s += str(int(t))
        s += '\n'
    print(s)


def init_level():
    tile_dict = None
    with open(JSON_MAP) as f:
        tile_dict = json.load(f)
    main_layer = tile_dict['layers'][0]
    data = main_layer['data']
    width = main_layer['width']
    height = main_layer['height']
    
    global tiles
    tiles = []
    for y in range(height):
        row = []
        for x in range(width):
            val = data[y*width + x] != 0
            row += [val];
        tiles += [row];


def overlaps(a1, a2, b1, b2):
    return max(a1, b1) <= min(a2, b2)

def outside_screen(width, height, new_x, new_y):
    return (new_x < 0 or
            new_x + width >= len(tiles[0]) * TILE_SIZE or
            new_y + height >= len(tiles) * TILE_SIZE)


def can_move_to(width, height, new_x, new_y, players={}):
    """
    Returns whether an entity with the given width and
    height can move to the given position. Also returns
    the player it collided with, if any.

    (int, int, int, int) -> (Player|None, bool)
    """

    if outside_screen(width, height, new_x, new_y):
        return None, False

    # Allow entities to exit the top of the screen to fall back
    if new_y >= 0:
        left = new_x // TILE_SIZE
        right = (new_x + width) // TILE_SIZE
        top = new_y // TILE_SIZE
        bottom = (new_y + height) // TILE_SIZE

        for y in range(top, bottom + 1):
            for x in range(left, right + 1):
                if tiles[y][x] != 0:
                    return (x * TILE_SIZE, y * TILE_SIZE), False
    
    for player in players.values():
        px, py = player.position
        if overlaps(new_x, new_x + width, px, px + PLAYER_WIDTH) and \
           overlaps(new_y, new_y + height, py, py + PLAYER_HEIGHT):
            return (player, False)

    return None, True


    
