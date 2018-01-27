
function initKeyboard() {
    keyD = game.input.keyboard.addKey(Phaser.Keyboard.D);
    keyD.onDown.add(keyDPressed, this);
    keyD.onUp.add(keyDReleased, this);
    keyA = game.input.keyboard.addKey(Phaser.Keyboard.A);
    keyA.onDown.add(keyAPressed, this);
    keyA.onUp.add(keyAReleased, this);
    keySpace = game.input.keyboard.addKey(Phaser.Keyboard.SPACEBAR);
    keySpace.onDown.add(keySpacePressed, this);
    keySpace.onUp.add(keySpaceReleased, this);
    game.input.keyboard.removeKeyCapture(Phaser.Keyboard.A);
    game.input.keyboard.removeKeyCapture(Phaser.Keyboard.D);
    game.input.keyboard.removeKeyCapture(Phaser.Keyboard.SPACEBAR);
    console.log('Keyboard initialized');
}

function initMouse() {
    game.input.onDown.add(onClick, this);
}

function onClick() {
    var x = game.input.activePointer.x;
    var y = game.input.activePointer.y;
    if (game.input.activePointer.rightButton.isDown) {
        mouseRightClicked(x, y);
    }
}

function keyAPressed() {
    sendKeystroke('left', true);
}

function keyAReleased() {
    sendKeystroke('left', false);
}

function keyDPressed() {
    sendKeystroke('right', true);
}

function keyDReleased() {
    sendKeystroke('right', false);
}

function keySpacePressed() {
    sendJump();
}

function keySpaceReleased() {
    console.log('SPACE RELEASED');
}

function mouseRightClicked(x, y) {
    console.log(x + ' ' + y);
}

function isLeftMouseButtonPressed() {
    return game.input.activePointer.leftButton.isDown;
}

