
// map of the player names mapped to their position, 
// health and sprite.
// Does not include this client's player
// name -> {x, y, health, sprite}

const AIM_POWER_SPEED = 0.1;

var time;

var map;
var layer;
var players = {};

var mainPlayerPosition;
var mainPlayerHealth;
var mainPlayerSprite;
var mainPlayerName;

var aiming;
var currentForce;
var currentAngle;
var aimCounter;

function initLevel() {
    aiming = false;
    currentForce = 0;
    aimCounter = 0;

    time = new Date().getTime();
    game.stage.backgroundColor = '#909090';
    
    map = game.add.tilemap('snowballMap');
    map.addTilesetImage('tileset', 'tileset');
    
    layer = map.createLayer('mapLayer');
    layer.resizeWorld();

    mainPlayerPosition = {x: 300, y: 300};
    mainPlayerHealth = 100;
    mainPlayerSprite = game.add.sprite(
            mainPlayerPosition.x,
            mainPlayerPosition.y,
            'snowman');
}

function updatePlayerPosition(name, x, y) {
    if (name == mainPlayerName) {
        mainPlayerPosition.x = x;
        mainPlayerPosition.y = y;
        mainPlayerSprite.x = x;
        mainPlayerSprite.y = y;
    }
    var p = players[name];
    p.x = x;
    p.y = y;
    p.sprite.x = x;
    p.sprite.y = y;
}

function addPlayers(playerList) {
    for (var i in playerList) {
        var name = playerList[i];
        var sprt = game.add.sprite(0, 0, 'snowman');
        players[name] = {x: 0, y: 0, health: 0, sprite: sprt};
    }
}

function getAngle(x1, y1, x2, y2) {
    return -Math.atan2((y2 - y1),(x2 - x1));
}

function levelUpdate() {
    var newTime = new Date().getTime();
    var deltaTime = (newTime - time)/30;
    time = newTime;
    
    if (isLeftMouseButtonPressed()) {
        aiming = true;
        currentForce = (-Math.cos(
                aimCounter*deltaTime*AIM_POWER_SPEED) + 1)/2;
        currentAngle = getAngle(mainPlayerPosition.x,
                    mainPlayerPosition.y, 
                    getMouseX(), getMouseY());
        console.log(currentForce);
        aimCounter++;
    } else {
        if (aiming) {
            sendFire(currentAngle, currentForce);
        }
        currentForce = 0;
        aiming = false;
        aimCounter = 0;
    }
}

