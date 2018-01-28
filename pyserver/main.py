#!/usr/bin/env python3

import threading
import asyncio
import player
import game
import level
import asyncio
import logging
import warnings
import websockets

level.init_level()
#logging.basicConfig(level=logging.DEBUG)

warnings.filterwarnings("default", '', ResourceWarning)

lobbies = {}

class Lobby:
    def __init__(self):
        self.clients = {}
        self.snowballs = {}
        self.thread = None

class Client:
    def __init__(self, websocket, player):
        self.websocket = websocket
        self.player = player

async def sockethandler(websocket, path):
    lobby_and_username = list(filter(None, path.split('/')))
    if len(lobby_and_username) != 2:
        print("invalid lobby/username path", lobby_and_username)
        await websocket.send("chat:(Server) lobby or user name is invalid")
        return

    lobby_name, username = lobby_and_username
    if lobby_name not in lobbies:
        lobbies[lobby_name] = Lobby()
        print("Creating new lobby", lobby_name)

    lobby = lobbies[lobby_name]

    if lobby.thread:
        print("This lobby ({}) is already playing a game. {} cannot join now.".format(lobby_name, username))
        await websocket.send("chat:(Server) The lobby {} is already playing a game. Choose a different lobby name".format(lobby_name))
        return

    lobby.clients[username] = Client(websocket, player.Player(username))
    print("{} has joined {}".format(username, lobby_name))

    client = lobby.clients[username]

    thread_stop_event = threading.Event()

    async def send_to_others(message):
        for client_name in lobby.clients:
            if client_name != username:
                await lobby.clients[client_name].websocket.send(message)

    async def broadcast(message):
        for client_name in lobby.clients:
            await lobby.clients[client_name].websocket.send(message)

    async def chat_handler(content):
        message_string = "({}) {}: {}".format(lobby_name, username, content)
        print(message_string)
        await send_to_others("chat:" + message_string)

    async def game_start_handler(content):
        if not lobby.thread:
            lobby.thread = threading.Thread(
                target = game.run_main_loop,
                args = (lobby, thread_stop_event, asyncio.get_event_loop()))
            lobby.thread.start()
            await broadcast("start game:" + " ".join(lobby.clients.keys()))

    async def key_down_handler(content):
        if content == 'left':
            client.player.left_pressed = True
        elif content == 'right':
            client.player.right_pressed = True
        else:
            print('Unknown key', content)

    async def key_up_handler(content):
        if content == 'left':
            client.player.left_pressed = False
        elif content == 'right':
            client.player.right_pressed = False
        else:
            print('Unknown key', content)

    async def jump_handler(content):
        vx, vy = client.player.velocity
        if client.player.on_ground:
            client.player.velocity = vx, vy - 20

    async def fire_handler(content):
        print('FIRE')
        angle, force = content.split(' ')
        player_pos = client.player.position
        game.create_snowball(lobby, player_pos, float(angle), float(force))

    message_handler = {
        "chat": chat_handler,
        "start game": game_start_handler,
        "key down": key_down_handler,
        "key up": key_up_handler,
        "jump": jump_handler,
        "fire": fire_handler
    }

    try:
        while True:
            message = await websocket.recv()
            split_message = message.split(':', 1)
            message_type = split_message[0]
            message_content = split_message[1] if len(split_message) > 1 else ""
            if message_type in message_handler:
                await message_handler[message_type](message_content)
            else:
                print("Unknown message type:", message)
    except websockets.exceptions.ConnectionClosed:
        pass

    print("({}) {} has disconnected".format(lobby_name, username))
    del lobby.clients[username]
    if not lobby.clients:
        print("The last client disconnected from {}. Shutting down that lobby.".format(lobby_name))
        if lobby.thread:
            thread_stop_event.set()
            lobby.thread.join()
        del lobbies[lobby_name]

asyncio.get_event_loop().run_until_complete(
    websockets.serve(sockethandler, 'localhost', 8765))

asyncio.get_event_loop().run_forever()
