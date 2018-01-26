
var players = {};

function mouseClicked(x, y) {
    console.log(x + ' ' + y);
}

function addPlayer() {
    // TODO implement
}

function levelUpdate() {
    if (cursors.left.isDown) {
        mainPlayer.position.x -= 1;
    } else if (cursors.right.isDown) {
        mainPlayer.position.x += 1;
    }
}

