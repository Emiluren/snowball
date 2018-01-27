import asyncio
import vec
import time
import player
import level
import util
import pdb

RUN_ACCELERATION = 1.0
GRAVITY_ACCELERATION = 1.3
PLAYER_MAX_SPEED = 10

def run_main_loop(lobby, stop_event, event_loop):
    running = True
    while not stop_event.is_set():
        # TODO: update player positions and broadcast new state
        update_players(lobby.clients)
        asyncio.run_coroutine_threadsafe(broadcast_positions(lobby.clients), event_loop)
        time.sleep(1/30)

async def broadcast_positions(clients):
    for username in clients:
        x, y = clients[username].player.position
        await util.broadcast(clients,
            'position:{} {} {}'.format(username, x, y))


def other_players(player, clients):
    return {p: v for p, v in clients.items() if p != player.name}


def update_player(player, clients):
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
    vx, vy = player.velocity
    _, can_move = level.can_move_to(level.PLAYER_WIDTH,
                                    level.PLAYER_HEIGHT,
                                    round(px + vx), py)
    if can_move:
        dx = (player.left_pressed*(-1) + player.right_pressed)
        new_vx = vx * 0.9 + dx
        if new_vx > PLAYER_MAX_SPEED:
            new_vx = PLAYER_MAX_SPEED * new_vx / new_vx
        player.velocity = (new_vx, vy)
        player.position = (round(px + vx), py)
    else:
        player.velocity = (0, vy)


def update_players(clients):
    for client_name in clients:
        player = clients[client_name].player
        update_player(player, clients)


def create_snowball_id(lobby):
    if not lobby.snowballs:
        return 0
    return max(lobby.snowballs.keys()) + 1


def create_snowball(lobby, pos, angle, force):
    ball_id = create_snowball_id(lobby)
    snowball = player.Snowball(ball_id)
    direction = vec.from_direction(angle, 1)
    velocity = vec.mul(direction, force*MAX_THROWING_FORCE)
    snowball.position = vec.add(pos, vec.mul(direction, SNOWBALL_SPAWN_DISTANCE))
    snowball.velocity = velocity
    lobby.snowballs[ball_id] = snowball
