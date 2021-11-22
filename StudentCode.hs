module StudentCode where

import Data.List
import Data.Maybe

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

-- a list where we keep all the active buildings of the board
type BuildingsList = [Building]

-- specifies which player currently makes a move
data Turn = Blue | Red deriving (Eq, Ord)
instance Show Turn where
  show Blue = "B"
  show Red = "R"


-- a complete specification of the game's state
data Game = Game {running :: Bool,
                  turn :: Turn,
                  bluePlayerPositions :: BluePlayerPositions,
                  redPlayerPositions :: RedPlayerPositions,
                  buildingsList :: BuildingsList,
                  moveHistory :: [Move],
                  moveFuture :: [Move]}


-- types to specify a move command given from the GUI
type InitialPosition = Position
type FinalPosition = Position
type BuildPosition = Position

data Move = Move {initialPosition :: InitialPosition,
                  finalPosition :: FinalPosition,
                  buildPosition :: BuildPosition} deriving (Eq, Show, Ord)


type Depth = Int
type Score = Int

-- given the positions, start with empty board and Blue plays first
initializeGame :: BluePlayerPositions -> RedPlayerPositions -> Game
initializeGame bluePlayers redPlayers = Game True Blue bluePlayers redPlayers [] [] []

-- wrapper because we must not assume structure of Move object for student
tryMove :: Game -> (Position, Position, Position) -> Game
tryMove game (initPos, finalPos, buildPos) = advanceGame game (Move {initialPosition = initPos, finalPosition = finalPos, buildPosition = buildPos})

-- should advance game by the specified move iff that move is legal
advanceGame :: Game -> Move -> Game
advanceGame game move
    | isLegalMove game move = applyMove game move
    | otherwise = game

checkForNoMoves :: Game -> Game
checkForNoMoves game 
    | null $ possibleMoves game = game {running = False}
    | otherwise = game

-- implement rules of legality
isLegalMove :: Game -> Move -> Bool
isLegalMove game move = running game &&                          -- game must not have ended 
                        playerPawnInInitialPosition game move && -- must start by moving current players pawn
                        positionsNeighboring move &&             -- should not move or build more than 1 to any direction, also guarantees movement
                        hasLegalJumpAndBuild game move &&        -- should not go up more than one level or rise to a dome, or build on a dome
                        doesNotLandOnPawn game move &&           -- should not land on another pawn, be it friend or foe, also guarantees movement
                        doesNotBuildOnPawn game move             -- should not build on top of other pawn

-- ensures that pawns do not overlap and pawns do not remain stationary (i.e. land on themselves)
doesNotLandOnPawn :: Game -> Move -> Bool
doesNotLandOnPawn game move = finPos `notElem` [bluePos1, bluePos2, redPos1, redPos2]
    where finPos = finalPosition move
          (bluePos1, bluePos2) = bluePlayerPositions game
          (redPos1, redPos2) = redPlayerPositions game

doesNotBuildOnPawn :: Game -> Move -> Bool
doesNotBuildOnPawn game move = all (\pos -> pos /= buildPos || pos == initPos) [bluePos1, bluePos2, redPos1, redPos2]
    where buildPos = buildPosition move
          initPos = initialPosition move
          (bluePos1, bluePos2) = bluePlayerPositions game
          (redPos1, redPos2) = redPlayerPositions game

-- ensures the initial position given actually has a pawn on it
playerPawnInInitialPosition :: Game -> Move -> Bool
playerPawnInInitialPosition game move = (playerPawnPos1 == initPos) || (playerPawnPos2 == initPos)
    where (playerPawnPos1, playerPawnPos2) = getCurrPlayerPositions game
          initPos = initialPosition move

-- ensures jumps are not long throughout the move, but also existent
positionsNeighboring :: Move -> Bool
positionsNeighboring move = closePositions initPos finPos && closePositions finPos buildPos
    where initPos = initialPosition move
          finPos = finalPosition move
          buildPos = buildPosition move

-- determines if a jump is not too long but also not the same 
closePositions :: Position -> Position -> Bool
closePositions (x1, y1) (x2, y2) = xDiff <= 1 && yDiff <= 1 && (xDiff + yDiff >= 1)
    where xDiff = abs (x1 - x2)
          yDiff = abs (y1 - y2)


-- checks if the player rises more than one level or rises to a dome
hasLegalJumpAndBuild :: Game -> Move -> Bool
hasLegalJumpAndBuild game move = (hFin - hInit) <= 1 && (hFin < 4) && buildTargetCurrentHeight < 4
    where hInit = maybeBuildingToHeight buildInit
          hFin = maybeBuildingToHeight buildFin
          buildTargetCurrentHeight = maybeBuildingToHeight buildTarget
          buildInit = findBuilding initPos buildings
          buildFin = findBuilding finPos buildings
          buildTarget = findBuilding buildPos buildings
          buildings = buildingsList game
          initPos = initialPosition move
          finPos = finalPosition move
          buildPos = buildPosition move

findBuilding :: Position -> [Building] -> Maybe Building
findBuilding posToFind = find (\(_, pos) -> pos == posToFind)

-- translates the result of a building search to the level of the ground where that search was looking at
maybeBuildingToHeight :: Maybe Building -> Int
maybeBuildingToHeight Nothing = 0
maybeBuildingToHeight (Just (height, _)) = height 

-- returns the positions of the pawns of the current player
getCurrPlayerPositions :: Game -> PlayerPositions
getCurrPlayerPositions game 
    | turn game == Blue = bluePlayerPositions game
    | otherwise = redPlayerPositions game



-- apply the move, given it is legal
applyMove :: Game -> Move -> Game
applyMove game move = checkForNoMoves $
                       game {running = stillRunning, 
                            turn = otherPlayer $ turn game,
                            bluePlayerPositions = newBluePlayerPositions,
                            redPlayerPositions = newRedPlayerPositions,
                            buildingsList = newBuildingsList,
                            moveHistory = move:history,
                            moveFuture = []}

        where newBluePlayerPositions = updatePlayerPositions initialPos finalPos $ bluePlayerPositions game -- update blue pawn's position, if blue moves
              newRedPlayerPositions = updatePlayerPositions initialPos finalPos $ redPlayerPositions game -- update red pawn's position, if red moves
              newBuildingsList = addFloor buildPos $ buildingsList game -- update the buildings
              buildingOnFinPos = find (\(_, pos) -> pos == finalPos) newBuildingsList -- find the building where player landed
              stillRunning = maybeBuildingToHeight buildingOnFinPos < 3 -- check if game won
              initialPos = initialPosition move
              finalPos = finalPosition move
              buildPos = buildPosition move
              history = moveHistory game


otherPlayer :: Turn -> Turn
otherPlayer Blue = Red
otherPlayer Red = Blue

updatePlayerPositions :: InitialPosition -> FinalPosition -> PlayerPositions -> PlayerPositions
updatePlayerPositions initialPos finalPos (pos1, pos2)
    -- we move pawn 1
    | initialPos == pos1 = (finalPos, pos2)
    -- we move pawn 2
    | initialPos == pos2 = (pos1, finalPos)
    | otherwise = (pos1, pos2)


updateBuildingsList :: Height -> Position -> BuildingsList -> BuildingsList
updateBuildingsList height buildPos buildings
    -- if no building at the given position
    | isNothing buildingFound = (1, buildPos):buildings
    -- if we got a building
    | otherwise = take buildingIndex buildings ++ resultingBuilding ++ drop (buildingIndex + 1) buildings -- modify the existing building's height
        where buildingFound = find (\(_, pos) -> pos == buildPos) buildings -- try to find a building in the list at that position
              justBuilding@(justHeight, justPos) = fromJust buildingFound -- get actual data for building found
              buildingIndex = fromJust $ elemIndex justBuilding buildings -- get the place in the list where we found the building
              newHeight = justHeight + height
              resultingBuilding = [(justHeight + height, justPos) | newHeight > 0] 
    

addFloor :: Position -> BuildingsList -> BuildingsList
addFloor = updateBuildingsList 1

removeFloor :: Position -> BuildingsList -> BuildingsList
removeFloor = updateBuildingsList (-1)

screenshotGame :: Game -> (Bool, Char, BluePlayerPositions, RedPlayerPositions, [Building])
screenshotGame game = (not $ running game, turnToDisplay, bluePlayerPositions game, redPlayerPositions game, buildingsList game)
    where turnToDisplay
            | running game = head $ show $ turn game
            | otherwise    = head $ show $ otherPlayer $ turn game

undoMove :: Game -> Game
undoMove game 
    | null history = game
    | otherwise = rewindedGame
        where moveToUndo = head history -- get the last move done
              history = moveHistory game  -- get the whole history of moves
              initPos = initialPosition moveToUndo -- get initial position
              finPos = finalPosition moveToUndo    -- get final position
              buildPos = buildPosition moveToUndo  -- get build position
              previousBuildings = buildingsList game -- get buildings as they are before the undo
              newBuildings = removeFloor buildPos previousBuildings -- undo move on buildings, just remove the floor built
              newBluePlayerPositions = updatePlayerPositions finPos initPos $ bluePlayerPositions game -- revert blue pawn's position, if blue moved
              newRedPlayerPositions = updatePlayerPositions finPos initPos $ redPlayerPositions game -- revert red pawn's position, if red moves
              newTurn = otherPlayer $ turn game
              newFuture = moveToUndo:moveFuture game
              rewindedGame = game {bluePlayerPositions = newBluePlayerPositions,
                                   redPlayerPositions = newRedPlayerPositions,
                                   moveHistory = tail history,
                                   buildingsList = newBuildings,
                                   turn = newTurn,
                                   moveFuture = newFuture,
                                   running = True}
                    
                
            
redoMove :: Game -> Game 
redoMove game 
    | null future = game
    | otherwise =  (applyMove game move) {moveFuture = tail future}
        where future = moveFuture game
              move = head future


possibleMoves :: Game -> [(Position, Position, Position)]
possibleMoves game = map toExplicitMove $ filter (\move -> isLegalMove game move && inBounds move) allMoves
    where currPlayer = turn game
          playingPositions 
            | currPlayer == Red = (\(pos1, pos2) -> [pos1, pos2]) $ redPlayerPositions game
            | otherwise = (\(pos1, pos2) -> [pos1, pos2]) $ bluePlayerPositions game
          offsets = [-1, 0, 1]
          possibleMovements = filter (\(x, y) -> abs x + abs y > 0) $ [(x, y) | x <- offsets, y <- offsets]
          allMoves = [toMyMove (playerPos, middlePos, endPos)| playerPos <- playingPositions, displacement1 <- possibleMovements, displacement2 <- possibleMovements, middlePos <- [addPositions playerPos displacement1], endPos <- [addPositions middlePos displacement2]]



toExplicitMove :: Move -> (Position, Position, Position)
toExplicitMove move = (initialPosition move, finalPosition move, buildPosition move)

inBounds :: Move -> Bool
inBounds move = posInBounds initPos && posInBounds finPos && posInBounds buildPos 
    where initPos = initialPosition move
          finPos = finalPosition move
          buildPos = buildPosition move

posInBounds :: Position -> Bool
posInBounds (row, col) = col >= 0 && col <= 4 && row >= 0 && row <= 4  

addPositions :: Position -> Position -> Position
addPositions (startRow, startCol) (displacementRow, displacementCol) = (startRow + displacementRow, startCol + displacementCol)




evaluateState :: Char -> Game -> Score
evaluateState playerChar game = buildingProximities + playerHeights
    where playerPositions = givenPlayerPositions player game
          enemyPositions = givenPlayerPositions (otherPlayer player) game
          buildings = buildingsList game
          playerHeights = myHeightScore - enemyHeightScore
          buildingProximities = myProx - enemyProx
          myProx = weightedDistancesToBuildings playerPositions buildings
          enemyProx = weightedDistancesToBuildings enemyPositions buildings
          myHeightScore = heightsScore playerPositions buildings
          enemyHeightScore = heightsScore enemyPositions buildings
          player = charToPlayer playerChar


heightsScore :: PlayerPositions -> [Building] -> Score
heightsScore (pos1, pos2) buildings = score1 + score2
    where score1 = weighPawnHeight $ maybeBuildingToHeight $ findBuilding pos1 buildings
          score2 = weighPawnHeight $ maybeBuildingToHeight $ findBuilding pos2 buildings 

weighPawnHeight :: Height -> Score
weighPawnHeight 3 = 100000
weighPawnHeight 2 = 150
weighPawnHeight 1 = 50
weighPawnHeight _ = 0

givenPlayerPositions :: Turn -> Game -> PlayerPositions
givenPlayerPositions player game
    | player == Blue = bluePlayerPositions game
    | otherwise = redPlayerPositions game

currentPlayerPositions :: Game -> PlayerPositions
currentPlayerPositions game
    | turn game == Blue = bluePlayerPositions game
    | otherwise = redPlayerPositions game


weightedDistancesToBuildings :: PlayerPositions -> [Building] -> Score
weightedDistancesToBuildings (pos1, pos2) buildings = sum $ map (\(buildHeight, buildPos) -> (closeness pos2 buildPos + closeness pos1 buildPos) * heightValue buildHeight) buildings


heightValue :: Height -> Int
heightValue 1 = 1
heightValue 2 = 3
heightValue 3 = 6
heightValue 4 = -10
heightValue _ = 0



closeness :: Position -> Position -> Int 
closeness (x1, y1) (x2, y2) = 10 - (abs (x1 - x2) + abs (y1 - y2))



toMyMove :: (Position, Position, Position) -> Move
toMyMove (initPos, finPos, buildPos) = Move initPos finPos buildPos


charToPlayer :: Char -> Turn
charToPlayer 'B' = Blue
charToPlayer _ = Red