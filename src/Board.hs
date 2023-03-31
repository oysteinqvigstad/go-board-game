module Board
    ( initBoard
    , addStoneToBoardAndRecord
    , opponent
    ) where

import Text
import Data.List (intersperse)
import Data.Maybe (isNothing)
import DataTypes


initBoard :: String -> World
initBoard str = world3
  where world = parser str
        b = placeStones (genEmptyBoard $ size world) $ setup world
        world2 = world { board = b }
        world3 = foldl addStoneToBoardWithoutRecord world2 $ moves world2
        -- initiateBoard creates a n*n empty board
        genEmptyBoard :: Int -> Board
        genEmptyBoard n = replicate n $ replicate n Nothing

        
-- printBoard writes a board to stdout
-- e.g.
--        X . . . .
--        . O . . .
--        . . . . .
--        . . . . .
--        . . . . .
printBoard :: Board -> IO ()
printBoard b = mapM_ (putStrLn . intersperse ' ') [ map stoneToChar row | row <- b ]
  where stoneToChar :: Maybe Stone -> Char
        stoneToChar (Just Black) = 'X'
        stoneToChar (Just White) = 'O'
        stoneToChar Nothing = '.'



removeStones :: Board -> [Move] -> Board
removeStones = foldl removeStone 

-- placeStone writes the stone to the board array
removeStone :: Board -> Move -> Board
removeStone b m = take (y m) b ++ [removeStone' (b !! y m)] ++ drop (y m + 1) b
  where removeStone' row = take (x m) row ++ [Nothing] ++ drop (x m + 1) row
        x move = fst $ coord move 
        y move = snd $ coord move
        
placeStones :: Board -> [Move] -> Board
placeStones = foldl placeStone


-- placeStone writes the stone to the board array
placeStone :: Board -> Move -> Board
placeStone b m = take (y m) b ++ [placeStone' (b !! y m)] ++ drop (y m + 1) b
  where placeStone' row = take (x m) row ++ [Just (stone m)] ++ drop (x m + 1) row
        x move = fst $ coord move 
        y move = snd $ coord move
        
        
legalCoord :: Board -> Move -> Bool
legalCoord b (Move _ (x, y)) =
  let xrange = [0..(length b - 1)]
      yrange = [0..(length (head b) - 1)]
  in x `elem` xrange && y `elem` yrange


removeIfNoLiberty :: (Int, Board) -> [Move] -> (Int, Board)
removeIfNoLiberty (score, b) m = if not $ hasLiberty m b then (score + length m, removeStones b m) else (score, b)





-- | Capture
-- >>> let w1 = initBoard "(;SZ[19];AW[aa][ab];AB[ba][bb];B[ac])"
-- >>> let w2 = initBoard "(;SZ[19];AB[ba][bb][ac])"
-- >>> board w1 == board w2
-- True
-- >>> (captured . last $ players w1) == 2
-- True
capture :: World -> Move -> World
capture s m =
  let b = board s
      neighbours = map ((Move . opponent . stone) m) $ getNeighborCoords (coord m) b
      removable = map (`getUnit` b) neighbours
      (num, board') = foldl removeIfNoLiberty (0, b) removable
  in s {players = addScore (players s) (stone m) num, board = board'}

suicide :: World -> Move -> World
suicide s m =
  let b = board s
      ownUnit = getUnit m b
      (num, board') = removeIfNoLiberty (0, b) ownUnit
  in s {players = addScore (players s) (opponent $ stone m) num, board = board'}
    
addStoneToBoardAndRecord :: World -> Move -> World
addStoneToBoardAndRecord s m = suicide (capture (s {moves = moves s ++ [m], board = placeStone b m}) m) m
  where b = board s

addStoneToBoardWithoutRecord :: World -> Move -> World
addStoneToBoardWithoutRecord s m = suicide (capture (s { board = placeStone b m}) m) m
  where b = board s

addScore :: [Player] -> Stone -> Int -> [Player]
addScore [p1, p2] s n = if s == White then [add' p1, p2] else [p1, add' p2]
  where add' p = p { captured = captured p + n }
addScore x _ _ = x

getUnit :: Move -> Board -> [Move]
getUnit m b = dfs [] m
  where color = stone m
        dfs :: [Move] -> Move -> [Move]
        dfs visited move
          | b !! y !! x /= Just color = visited
          | move `elem` visited = visited
          | otherwise = foldl dfs (move : visited) neighbors
          where (x, y) = coord move
                neighbors = map (Move color) $ getNeighborCoords (coord move) b

hasLiberty :: [Move] -> Board -> Bool
hasLiberty m b = any isNothing intersects
  where neighbors = concatMap (\x -> getNeighborCoords (coord x) b) m
        intersects = map (\x -> b !! snd x !! fst x) neighbors

getNeighborCoords :: Coord -> Board -> [Coord]
getNeighborCoords (x, y) b = filter (legalCoord b . Move Black) [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

opponent :: Stone -> Stone
opponent s = if s == Black then White else Black

