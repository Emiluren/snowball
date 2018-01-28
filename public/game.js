function appendOutput(cls, text) {
    $('#console-output').append('<pre class="' + cls + '">' + text + '</pre>');
    $('#line').focus();
}

var game;
var ws;

const JUMP_AUDIO = 1
const HIT_GROUND_AUDIO = 2
const HIT_PLAYER_AUDIO = 3
const THROW_AUDIO = 4
const DEATH_AUDIO = 5

var sounds;

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
        },
        "play": function(messageContent) {
            var sound = messageContent;
            playSound(sound);
        }
    }

    $('#login').submit(function () {
        var lobby = $('#lobby').val();
        var username = $('#username').val();
        mainPlayerName = username;
        $('#login').css('display', 'none');
        $('#console').css('display', 'block');

        ws = new WebSocket("ws://" + location.hostname + ":30000/" + lobby + '/' + username);
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

function initAudio() {
    sounds = {};
    sounds[JUMP_AUDIO] = [
        game.add.audio('jump1'),
        game.add.audio('jump2'),
        game.add.audio('jump3'),
        game.add.audio('jump4')
    ];
    sounds[HIT_GROUND_AUDIO] = [
        game.add.audio('hitground1'),
        game.add.audio('hitground2'),
        game.add.audio('hitground3'),
        game.add.audio('hitground4')
    ];
    sounds[HIT_PLAYER_AUDIO] = [
        game.add.audio('hitplayer1'),
        game.add.audio('hitplayer2'),
        game.add.audio('hitplayer3'),
        game.add.audio('hitplayer4')
    ];
    sounds[THROW_AUDIO] = [
        game.add.audio('throw1'),
        game.add.audio('throw2'),
        game.add.audio('throw3')
    ];
    sounds[DEATH_AUDIO] = [
        game.add.audio('death1'),
        game.add.audio('death2'),
        game.add.audio('death3')
    ];
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
    game.load.image('healthbar-red', 'assets/healthbar-red.png');
    
    game.load.bitmapFont('carrier_command', 'assets/carrier_command.png', 'assets/carrier_command.xml');

    game.load.audio('jump1', 'assets/audio/jump1.ogg');
    game.load.audio('jump2', 'assets/audio/jump2.ogg');
    game.load.audio('jump3', 'assets/audio/jump3.ogg');
    game.load.audio('jump4', 'assets/audio/jump4.ogg');

    game.load.audio('hitground1', 'assets/audio/hitground1.ogg');
    game.load.audio('hitground2', 'assets/audio/hitground2.ogg');
    game.load.audio('hitground3', 'assets/audio/hitground3.ogg');
    game.load.audio('hitground4', 'assets/audio/hitground4.ogg');

    game.load.audio('hitplayer1', 'assets/audio/hitplayer1.ogg');
    game.load.audio('hitplayer2', 'assets/audio/hitplayer2.ogg');
    game.load.audio('hitplayer3', 'assets/audio/hitplayer3.ogg');
    game.load.audio('hitplayer4', 'assets/audio/hitplayer4.ogg');

    game.load.audio('throw1', 'assets/audio/throw1.ogg');
    game.load.audio('throw2', 'assets/audio/throw2.ogg');
    game.load.audio('throw3', 'assets/audio/throw3.ogg');

    game.load.audio('death1', 'assets/audio/death1.ogg');
    game.load.audio('death2', 'assets/audio/death2.ogg');
    game.load.audio('death3', 'assets/audio/death3.ogg');

    game.stage.disableVisibilityChange = true;
}

function create() {
    initAudio();
    initLevel();
    initKeyboard();
    initMouse();
    initText();
}

function update() {
    if (!gameOver) {
        levelUpdate();
    }
}
