# Introduction 

This is a Haskell implementation of the board game "Santorini", which is described below.

It was developed as a final project for the course "Principles of Programming Languages" of the department of Informatics and
Telecommunications at the National and Kapodistrian University of Athens. 

If you would like to try and carry out the project, delete StudentCode.hs, rename cleanStudentCode.hs to StudentCode.hs and fill in the
required functions.

# Santorini

![Santorini photo](https://upload.wikimedia.org/wikipedia/commons/thumb/8/85/Santorini_board_game.jpg/1024px-Santorini_board_game.jpg)

In the very beginning, each of the two players places their two pawns on the 5-by-5 board.

Each turn of play involves moving one of your two pieces around the board and then placing a tile adjacent to the moved piece, building up that spot of the board. On subsequent turns, pieces may be moved onto one of these built-up tiles, but only one level up at a time. Pieces may also be moved down any number of levels. Players may also place a special dome tile on top of a three-level building, which prevents a player from moving onto that spot for the remainder of the game.

The primary winning condition is to get one of your pieces onto the third level, though players may also win if their opponent is unable to make a move.

More information about the game can be found in https://en.wikipedia.org/wiki/Santorini_(game).

# The assignment's requirements
An english version of the programming assignment details given to the students can be found in the file [Assignment.pdf](./Assignment.pdf)

# Setup Instructions for Linux
The graphics library gloss is necessary for the visualization to run. It can be installed using cabal. Also, the gloss library 
(and therefore the project) relies on the openGL module, which should be installed in the system. Compilation is carried out 
with the command "ghc ./visualization.hs".

# Setup instructions for Windows
The same process as for Linux should be followed, with haskell-platform installed on the system. The openGL module (glut32.dll) 
is provided in the folder for ease.

# How to play 
Right after the board loads, 4 tiles must be chosen by clicking for the initial positions of the pawns. After that, the first click 
indicates the pawn to move, the second one the tile to move it to, and the third and final click specifies the position on top of 
which to build. Illegal moves are not carried out by the system, so don't worry if you misclick. 

# Undo/Redo
The Z and Y buttons trigger undo and redo actions respectively. These undos/redos act on the move history as one would expect.

# AI player
Pressing A toggles the usage of an AI player. If AI mode is on, after every human move, a corresponding AI move is calculated and
carried out. The + and - buttons adjust the search depth of the AI player.
