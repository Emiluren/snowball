# Communication protocol

These are the messages sent between the server and clients.

## Server to client

| Type          | Content                       |
|:-------------:|-------------------------------|
| position      | x y                           |
| start game    | name1 name2 ...               |
| snowballs     | id1 x1 y1 id2 x2 y2 ...       |
| health        | name health                   |
| delete ball   | id                            |
| play          | audionumber                   |


## Client to server

| Type      | Content                       |
|:---------:|-------------------------------|
| jump      |                               |
| key down  | left/right                    |
| key up    | left/right                    |
| fire      | angle(pi to -pi) force(0 to 1)|
| new ball  |                               |

