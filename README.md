# WIP for assignment 1

Just a private workspace for working on concepts and testing things out

## TODO

- [x] Stone capturing
  - [x] implement DFS
    - [x] return list of adjecent stones
    - [x] check if group have liberties
  - [x] check for captures
    - [x] remove captures if applicable
    - [x] check for suicide
    - [x] count number of captures (score)
- [ ] parse SGF
  - [ ] read SGF from file
  - [x] read SGF string into data structure
  - [x] apply data structure
  - [x] add new moves to data structure
  - [ ] write data structure to file
- [ ] additional checks for illegal moves
  - [ ] Ko fight 
- [ ] create a graphical interface
  - [x] create context/window
  - [x] rewrite relative coordinate system for square board
  - [x] create a resizeable grid
  - [x] draw stones to grid 
  - [x] scale the size of stones
  - [x] include textures into World
  - [x] alternate players
  - [ ] add text support
    - [x] initialize text library
    - [x] define area for player info / stats
    - [x] write player names
    - [x] write scores
    - [ ] draw stone texture besides name
    - [ ] draw stones as scores
    - [ ] draw indicator for player turn
  - [ ] mouse interaction
    - [x] get coordinates from screen
    - [x] draw intersect indicator
    - [x] place stone on click 
    - [x] set position to (0,0) when resizing