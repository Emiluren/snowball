import vec
import time
import player

GRAVITY_ACCELERATION = 1

def run_main_loop(lobby):
    running = True
    while running:
        # TODO: update player positions and broadcast new state
        time.sleep(1/30)


def other_players(player, lobby):
    return {p: v for p, v in lobby.items() if p != player.name}


def update_player(player, lobby):
    px, py = player.position
    vx, vy = player.velocity
    if can_move_to(player.PLAYER_WIDTH, player.PLAYER_HEIGHT, 
                   px, round(py + vy)):
        player.velocity = (vx, vy + vy*GRAVITY_ACCELERATION)
        player.position = (px, py + vy)
    else:
        player.velocity = (vx, 0);

    px, py = player.position
    dx = player.left_pressed*(-1) + player.right_pressed

    if can_move_to(player.PLAYER_WIDTH, player.PLAYER_HEIGHT, 
                   px + dx, py):
        player.position = (px + dx, py)





def update_players(lobby):
    for client_name in lobby:
        player = lobby[client_name].player
        update_player(player, lobby)

