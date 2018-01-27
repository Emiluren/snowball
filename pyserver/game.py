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
SNOWBALL_SIZE = 10
SNOWBALL_DAMAGE = 1

RUN_ACCELERATION = 1.0
GRAVITY_ACCELERATION = 1.3
PLAYER_MAX_SPEED = 10

def run_main_loop(lobby, stop_event, event_loop):
    running = True
    print('hej')
    while not stop_event.is_set():
        # TODO: update player positions and broadcast new state
        # pdb.set_trace()
        update_players(lobby)
        print('kebab')
        # changed_healths = update_snowballs(lobby)
        
        # if changed_healths:
        #     asyncio.run_coroutine_threadsafe(
        #         broadcast_health(lobby, changed_healths),
        #         event_loop)

        # asyncio.run_coroutine_threadsafe(
        #     broadcast_snowballs(lobby), event_loop)

        asyncio.run_coroutine_threadsafe(
            broadcast_positions(lobby), event_loop)

        time.sleep(1/30)


async def broadcast_snowballs(lobby):
    message = 'snowballs:'
    for snowball in lobby.snowballs.values():
        x, y = snowball.position
        message += '{} {} {};'.format(snowball.id, x, y)
    await util.broadcast(lobby, message)
    

async def broadcast_health(lobby, changed_healths):
    message = 'health:'
    for name, health in changed_healths:
        await util.broadcast(lobby, 
                             message + name + ' ' + str(health))


async def broadcast_positions(lobby):
    for username in lobby.clients:
        x, y = lobby.clients[username].player.position
        await util.broadcast(lobby,
            'position:{} {} {}'.format(username, x, y))


def other_players(player, lobby):
    return {p: v for p, v in lobby.clients.items() 
            if p != player.name}


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
    for client_name in lobby.clients:
        player = lobby.clients[client_name].player
        update_player(player, lobby)


def update_snowballs(lobby):
    changed_healths = []
    destroyed_snowballs = []
    for id, snowball in lobby.snowballs.items():
        new_vel = vec.add(snowball.velocity, 
                          (0, GRAVITY_ACCELERATION))

        new_pos = vec.add(snowball.position, new_vel)
        new_x, new_y = new_pos
        player, can_move = can_move_to(SNOWBALL_SIZE,
                                       SNOWBALL_SIZE,
                                      round(new_x), round(new_y),
                                      other_players(lobby))
        if can_move:
            snowball.velocity = new_vel
            snowball.position = new_pos
        else:
            speed = vec.length(snowball.velocity)
            player.health = max(0, player.health - 
                                speed*SNOWBALL_DAMAGE)
            changed_healths.append(player)
            destroyed_snowballs.append(id)
    lobby.snowballs = {id: v 
                       for id, v in lobby.snowballs.items()
                      if id not in destroyed_snowballs}
    return changed_healths


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

