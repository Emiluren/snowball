import json
import pdb

JSON_MAP = '../public/assets/map.json'

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
    tiles = [[data[i*width + j] != 0 
              for j in range(width)] for i in range(height)]


def overlaps(a1, a2, b1, b2):
    return max(a1, a2) <= min(b1, b2) or \
           max(b1, b2) <= min(a1, a2)


def can_move_to(width, height, new_x, new_y, players={}):
    """
    Returns whether an entity with the given width and
    height can move to the given position. Also returns
    the player it collided with, if any.

    (int, int, int, int) -> (Player|None, bool)
    """
    covered_tiles = map(lambda row: row[new_x:new_x+width], 
                        tiles[new_y:new_y+height])
    
    for row in covered_tiles:
        if any(row):
            return None, False 
    
    for player in players.values():
        px, py = player.position
        if overlaps(new_x, new_x + width, px, px + PLAYER_WIDTH) and \
           overlaps(new_y, new_y + height, py, py + PLAYER_HEIGHT):
            return (player, False)

    return None, True


    
