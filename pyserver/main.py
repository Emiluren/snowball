#!/usr/bin/env python3

import level
import asyncio
import websockets

level.init_level()

lobbies = {}

async def echo(websocket, path):
    lobby_and_username = filter(None, path.split('/'))
    if len(lobby_and_username) != 2:
        print("invalid lobby/username path", lobby_and_username)
    else:
        lobby_name, username = lobby_and_username
        if lobby_name not in lobbies:
            lobbies[lobby_name] = {}
            print("Creating new lobby", lobby_name)

        async for message in websocket:
            await websocket.send(message)

asyncio.get_event_loop().run_until_complete(
    websockets.serve(echo, 'localhost', 8765))

asyncio.get_event_loop().run_forever()
