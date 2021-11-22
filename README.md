# Santorini

This is a Haskell implementation of the board game "Santorini", described here https://en.wikipedia.org/wiki/Santorini_(game)

It was developed as final project for the course "Principles of Programming Languages" of the department of Informatics and
Telecommunications at the National and Kapodistrian University of Athens. 

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
