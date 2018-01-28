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
            var contentsSplit = messageContent.split(' ');
            var name = contentsSplit[0];
            var x = contentsSplit[1];
            var y = contentsSplit[2];
            updatePlayerPosition(name, x, y);
        },
        "snowballs": function(messageContent) {
            var contentsSplit = messageContent.split(';');
            for (var i in contentsSplit) {
                var data = contentsSplit[i].split(' ');
                var id = data[0];
                var x = data[1];
                var y = data[2];
                updateSnowball(id, x, y);
            }
        },
        "health": function(messageContent) {
            var contentsSplit = messageContent.split(' ');
            var name = contentsSplit[0];
            var health = contentsSplit[1];
            
            updateHealth(name, health);
        },
        "delete ball": function (messageContent) {
            var id = messageContent;
            
            deleteSnowball(id);
        }
    }

    $('#login').submit(function () {
        var lobby = $('#lobby').val();
        var username = $('#username').val();
        mainPlayerName = username;
        $('#login').css('display', 'none');
        $('#console').css('display', 'block');

        ws = new WebSocket("ws://localhost:8765/" + lobby + '/' + username);
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

function sendNewSnowball() {
    ws.send('new ball');
}

function preload() {
    game.load.tilemap('snowballMap', 'assets/map.json', null, Phaser.Tilemap.TILED_JSON);
    game.load.image('tileset', 'assets/tileset.png');
    game.load.image('snowman', 'assets/snowman.png');
    game.load.image('arrow', 'assets/arrow.png');
    game.load.image('powerbar', 'assets/powerbar.png');
    game.load.image('snowball', 'assets/snowball.png');
    game.load.image('healthbar', 'assets/healthbar.png');
    game.load.image('healthbar-main', 'assets/healthbar-main.png');
    
    game.load.bitmapFont('carrier_command', 'assets/carrier_command.png', 'assets/carrier_command.xml');
    game.stage.disableVisibilityChange = true;
}

function create() {
    initLevel();
    initKeyboard();
    initMouse();
}

function update() {
    levelUpdate();
}
