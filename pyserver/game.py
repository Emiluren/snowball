import asyncio
import vec
import time
import player
import level
import util

GRAVITY_ACCELERATION = 1

def run_main_loop(lobby, event_loop):
    running = True
    while running:
        # TODO: update player positions and broadcast new state
        update_players(lobby)
        print('NEJ')
        asyncio.run_coroutine_threadsafe(broadcast_positions(lobby), event_loop)
        print('JO')
        time.sleep(1/30)

async def broadcast_positions(lobby):
    for username in lobby:
        x, y = lobby[username].player.position
        await util.broadcast(lobby,
            'position: {} {} {}'.format(username, x, y))


def other_players(player, lobby):
    return {p: v for p, v in lobby.items() if p != player.name}


def update_player(player, lobby):
    px, py = player.position
    vx, vy = player.velocity
    if level.can_move_to(level.PLAYER_WIDTH, level.PLAYER_HEIGHT, 
                   px, round(py + vy)):
        player.velocity = (vx, vy + vy*GRAVITY_ACCELERATION)
        player.position = (px, py + vy)
    else:
        player.velocity = (vx, 0);

    px, py = player.position
    dx = player.left_pressed*(-1) + player.right_pressed

    if level.can_move_to(level.PLAYER_WIDTH, level.PLAYER_HEIGHT, 
                   px + dx, py):
        player.position = (px + dx, py)


def update_players(lobby):
    for client_name in lobby:
        player = lobby[client_name].player
        update_player(player, lobby)


