function appendOutput(cls, text) {
    $('#console-output').append('<pre class="' + cls + '">' + text + '</pre>');
    $('#line').focus();
}

$(document).ready(function () {
    var ws;

    $('#username').focus();

    var messageHandler = {
        "chat": function(messageContent) {
            appendOutput('otherMessage', messageContent + '\n');
        },
        "start game": function(messageContent) {
            //var game = new Phaser.Game(800, 600, Phaser.AUTO, '', { preload: preload, create: create, update: update });
        }
    }

    $('#login').submit(function () {
        var lobby = $('#lobby').val();
        var username = $('#username').val();
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
                messageHandler[messageType](messageHandler);
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

function preload() {
}

function create() {
}

function update() {
}
