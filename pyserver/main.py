#!/usr/bin/env python3

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


class Client:
    def __init__(self, websocket, player):
        self.websocket = websocket
        self.player = player


async def sockethandler(websocket, path):
    lobby_and_username = list(filter(None, path.split('/')))
    if len(lobby_and_username) != 2:
        print("invalid lobby/username path", lobby_and_username)
    else:
        lobby_name, username = lobby_and_username
        if lobby_name not in lobbies:
            lobbies[lobby_name] = {}
            print("Creating new lobby", lobby_name)

        # TODO: add disconnect handling

        lobby = lobbies[lobby_name]
        lobby[username] = Client(websocket, player.Player(username))

        client = lobby[username]

        async def send_to_others(message):
            for client_name in lobby:
                if client_name != username:
                    await lobby[client_name].websocket.send(message)

        async def broadcast(message):
            for client_name in lobby:
                await lobby[client_name].websocket.send(message)

        async def chat_handler(content):
            message_string = "({}) {}: {}".format(lobby_name, username, content)
            print(message_string)
            await send_to_others("chat:" + message_string)

        async def game_start_handler(content):
            threading.Thread(target=game.run_main_loop, args=(lobby,))
            await broadcast("start game:" + " ".join(lobby.keys()))

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

        message_handler = {
            "chat": chat_handler,
            "start game": game_start_handler,
            "key down": key_down_handler,
            "key up": key_up_handler
        }

        async for message in websocket:
            split_message = message.split(':', 1)
            message_type = split_message[0]
            message_content = split_message[1] if len(split_message) > 1 else ""
            if message_type in message_handler:
                await message_handler[message_type](message_content)
            else:
                print("Unknown message type:", message)

asyncio.get_event_loop().run_until_complete(
    websockets.serve(sockethandler, 'localhost', 8765))

asyncio.get_event_loop().run_forever()
