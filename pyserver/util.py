
async def broadcast(lobby, message):
    for client_name in lobby:
        await lobby[client_name].websocket.send(message)
    print(message)


async def send_message(client, message):
    await client.websocket.send(message)


