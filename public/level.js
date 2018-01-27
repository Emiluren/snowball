
// map of the player names mapped to their position, 
// health and sprite.
// Does not include this client's player
// name -> {x, y, health, sprite}
var map;
var layer;
var players = {};

var mainPlayerPosition;
var mainPlayerHealth;
var mainPlayerSprite;
var mainPlayerName;

function initLevel() {
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

    for (var i in playerList) {
        var name = playerList[i];
        if (name != mainPlayerName) {
            players[name].sprite = game.add.sprite(0, 0, 'snowman');
        } else {
            players[name].sprite = mainPlayerSprite;
        }
    }
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
        players[name] = {x: 0, y: 0, health: 0, sprite: null};
    }
}

function levelUpdate() {
    
}

