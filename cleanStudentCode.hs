module StudentCode where


-- data types that specify a pawn's place on the board
type RowPos = Int
type ColumnPos = Int
type Position = (RowPos, ColumnPos)

-- positions for players' pawns
type PlayerPositions = (Position, Position)
type BluePlayerPositions = PlayerPositions
type RedPlayerPositions = PlayerPositions

-- types that specify a building
type Height = Int
type Building = (Height, Position)
type BuildingsList = [Building]

type Turn = Char
type Depth = Int

-- a complete specification of the game's state
data Game = Game


-- given the positions, start with specified pawns on board and Blue plays first
initializeGame :: BluePlayerPositions -> RedPlayerPositions -> Game
initializeGame = undefined

-- apply specified move (pawn initial position, pawn final position, pawn build position) if and only if it is legal 
tryMove :: Game -> (Position, Position, Position) -> Game
tryMove = undefined

-- returns (game has ended, player character, blue positions, red positions, list with buildings)
screenshotGame :: Game -> (Bool, Turn, BluePlayerPositions, RedPlayerPositions, BuildingsList)
screenshotGame = undefined

-- rewind last move executed on input game
undoMove :: Game -> Game
undoMove = undefined

-- redo last move undone on input game  
redoMove :: Game -> Game 
redoMove = undefined

-- list all possible legal moves the current player can make in the game
possibleMoves :: Game -> [(Position, Position, Position)]
possibleMoves = undefined

-- estimate winning probability for given player and game
evaluateState :: Turn -> Game -> Int
evaluateState = undefined