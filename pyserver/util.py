
async def broadcast(lobby, message):
    for client_name in lobby.clients:
        await lobby[client_name].websocket.send(message)
    print("broadcast", message)

