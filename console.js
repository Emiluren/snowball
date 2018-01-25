function appendOutput(cls, text) {
    $('#console-output').append('<pre class="' + cls + '">' + text + '</pre>');
    $('#line').focus();
}

$(document).ready(function () {
    var ws;

    $('#username').focus();

    $('#login').submit(function () {
        var username = $('#username').val();
        $('#login').css('display', 'none');
        $('#console').css('display', 'block');

        ws = new WebSocket("ws://localhost:8000/chat/" + username);
        appendOutput('stderr', 'Opening WebSockets connection...\n');

        ws.onerror = function(event) {
            appendOutput('stderr', 'WebSockets error: ' + event.data + '\n');
        };

        ws.onopen = function() {
            appendOutput('stderr', 'WebSockets connection successful!\n');
        };

        ws.onclose = function() {
            appendOutput('stderr', 'WebSockets connection closed.\n');
        };

        ws.onmessage = function(event) {
            appendOutput('stdout', event.data + '\n');
        };

        return false;
    });

    $('#console-input').submit(function () {
        var line = $('#line').val();
        ws.send(line);
        appendOutput('stdin', line + '\n');
        $('#line').val('');
        return false;
    });
});
