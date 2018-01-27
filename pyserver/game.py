import asyncio
import vec
import time
import player
import level
import util
import pdb

GRAVITY_ACCELERATION = 0.6
MAX_THROWING_FORCE = 10
SNOWBALL_SPAWN_DISTANCE = 30;

def run_main_loop(lobby, stop_event, event_loop):
    running = True
    while not stop_event.is_set():
        # TODO: update player positions and broadcast new state
        update_players(lobby)
        asyncio.run_coroutine_threadsafe(broadcast_positions(lobby), event_loop)
        time.sleep(1/30)

async def broadcast_positions(lobby):
    for username in lobby:
        x, y = lobby[username].player.position
        await util.broadcast(lobby,
            'position:{} {} {}'.format(username, x, y))


def other_players(player, lobby):
    return {p: v for p, v in lobby.items() if p != player.name}


def update_player(player, lobby):
    # pdb.set_trace()
    px, py = player.position
    vx, vy = player.velocity
    _, can_move = level.can_move_to(level.PLAYER_WIDTH, 
                                    level.PLAYER_HEIGHT,
                                    px, round(py + vy))
    if can_move:
        player.velocity = (vx, vy + GRAVITY_ACCELERATION)
        player.position = (px, round(py + vy))
    else:
        player.velocity = (vx, 0);

    px, py = player.position
    dx = (player.left_pressed*(-1) + player.right_pressed) * 10

    _, can_move = level.can_move_to(level.PLAYER_WIDTH, 
                                    level.PLAYER_HEIGHT, 
                                    px + dx, py)
    if can_move:
        player.position = (px + dx, py)


def update_players(lobby):
    for client_name in lobby:
        player = lobby[client_name].player
        update_player(player, lobby)


def update_snowballs(lobby):
    pass


def create_snowball_id(lobby):
    if not lobby.snowballs:
        return 0
    return max(lobby.snowballs.keys()) + 1


def create_snowball(lobby, pos, angle, force):
    ball_id = create_snowball_id(lobby)
    snowball = Snowball(ball_id)
    direction = vec.from_direction(angle, 1)
    velocity = vec.mul(direction, force*MAX_THROWING_FORCE)
    snowball.position = vec.add(pos, vec.mul(direction, SNOWBALL_SPAWN_DISTANCE))
    snowball.velocity = velocity
    lobby.snowballs[ball_id] = snowball

