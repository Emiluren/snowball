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


def update_players(lobby):
    for client_name in lobby:
        player = lobby[client_name].player
        update_player(player, lobby)


