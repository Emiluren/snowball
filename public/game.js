function appendOutput(cls, text) {
    $('#console-output').append('<pre class="' + cls + '">' + text + '</pre>');
    $('#line').focus();
}

var game;
var ws;

$(document).ready(function () {

    $('#username').focus();

    var messageHandler = {
        "chat": function(messageContent) {
            appendOutput('otherMessage', messageContent + '\n');
        },
        "start game": function(messageContent) {
            var callbacks = { preload: preload, create: create, update: update };
            game = new Phaser.Game(800, 600, Phaser.AUTO, 'game-container', callbacks);
            playerList = messageContent.split(' ');
            addPlayers(playerList);
        },
        "position": function(messageContent) {
            contentsSplit = messageContent.split(' ');
            name = contentsSplit[0];
            x = contentsSplit[1];
            y = contentsSplit[2];
            updatePlayerPosition(name, x, y);
        }
    }

    $('#login').submit(function () {
        var lobby = $('#lobby').val();
        var username = $('#username').val();
        mainPlayerName = username;
        $('#login').css('display', 'none');
        $('#console').css('display', 'block');

        ws = new WebSocket("ws://localhost:8000/chat/" + lobby + '/' + username);
        appendOutput('status', 'Opening WebSockets connection...\n');

        ws.onerror = function(event) {
            appendOutput('status', 'WebSockets error: ' + event.data + '\n');
        };

        ws.onopen = function() {
            appendOutput('status', 'WebSockets connection successful!\n');
        };

        ws.onclose = function() {
            appendOutput('status', 'WebSockets connection closed.\n');
        };

        ws.onmessage = function(event) {
            var splitMessage = event.data.split(/:(.+)/);
            var messageType = splitMessage[0];
            var messageContent = splitMessage[1];

            if (messageType in messageHandler) {
                messageHandler[messageType](messageContent);
            } else {
                console.log("Received unknown message type: " + event.data);
            }
        };

        return false;
    });

    $('#console-input').submit(function() {
        var line = $('#line').val();
        ws.send("chat:" + line);
        appendOutput('myMessage', line + '\n');
        $('#line').val('');
        return false;
    });

    $('#game-starter').submit(function() {
        ws.send("start game");
        return false;
    });
});

function sendKeystroke(stroke, down) {
    if (down) {
        ws.send('key down:' + stroke);
    } else {
        ws.send('key up:' + stroke);
    }
}

function sendFire(angle, force) {
    ws.send('fire:' + angle + ' ' + force);
}

function sendJump() {
    ws.send('jump');
}

function preload() {
    game.load.tilemap('snowballMap', 'assets/map.json', null, Phaser.Tilemap.TILED_JSON);
    game.load.image('tileset', 'assets/tileset.png');
    game.load.image('snowman', 'assets/snowman.png');
    game.load.image('arrow', 'assets/arrow.png');
}

function create() {
    initLevel();
    initKeyboard();
    initMouse();
}

function update() {
    levelUpdate();
}
