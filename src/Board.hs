module Board
    ( initBoard
    , addStoneToBoardAndRecord
    , opponent
    ) where

import Text
import Data.Maybe (isNothing)
import DataTypes

-- | initBoard takes a SGF formated string and produces the game state
-- >>> let w = initBoard "(;SZ[13]PW[White];AB[aa];W[ba][ab])"
-- >>> size w == 13
-- True
-- >>> head (players w) == Player {name = "White", captured = 1}
-- True
initBoard :: String -> World
initBoard str = world3
  where world = parser str
        b = placeStones (genEmptyBoard $ size world) $ setup world
        world2 = world { board = b, prevBoard = b }
        world3 = foldl addStoneToBoardWithoutRecord world2 $ moves world2
        -- initiateBoard creates a n * n empty board
        genEmptyBoard :: Int -> Board
        genEmptyBoard n = replicate n $ replicate n Nothing

        

-- | removeStones removes stones from the board array
-- >>> let b1 = board $ initBoard "(;SZ[13];B[aa][cc])"
-- >>> let b2 = board $ initBoard "(;SZ[13])"
-- >>> let ms = [Move {stone = Black, coord = (0,0)}, Move {stone = Black, coord = (2,2)}]
-- >>> removeStones b1 ms == b2
-- True
-- >>> removeStones b1 [head ms] == b2
-- False
removeStones :: Board -> [Move] -> Board
removeStones = foldl removeStone 


-- | removeStone removes a single stone to the board array
-- >>> let b1 = board $ initBoard "(;SZ[13];B[aa])"
-- >>> let b2 = board $ initBoard "(;SZ[13])"
-- >>> let m1 = Move {stone = Black, coord = (0,0)}
-- >>> let m2 = Move {stone = Black, coord = (0,1)}
-- >>> removeStone b1 m1 == b2
-- True
-- >>> removeStone b1 m2 == b2
-- False
-- >>> removeStone b2 m2 == b2
-- True
removeStone :: Board -> Move -> Board
removeStone b m = take (y m) b ++ [removeStone' (b !! y m)] ++ drop (y m + 1) b
  where removeStone' row = take (x m) row ++ [Nothing] ++ drop (x m + 1) row
        x move = fst $ coord move 
        y move = snd $ coord move
        
        
-- | placeStones places multiple stones to the board array
-- >>> let b1 = board $ initBoard "(;SZ[13])"
-- >>> let b2 = board $ initBoard "(;SZ[13];B[bb];W[bc])"
-- >>> let ms = [Move {stone = Black, coord = (1,1)}, Move {stone = White, coord = (1,2)}]
-- >>> placeStones b1 ms == b2
-- True
placeStones :: Board -> [Move] -> Board
placeStones = foldl placeStone


-- | placeStone writes the stone to the board array
-- >>> let b1 = board $ initBoard "(;SZ[13])"
-- >>> let b2 = board $ initBoard "(;SZ[13];B[aa])"
-- >>> let m = Move {stone = Black, coord = (0,0)}
-- >>> placeStone b1 m == b2
-- True
placeStone :: Board -> Move -> Board
placeStone b m = take (y m) b ++ [placeStone' (b !! y m)] ++ drop (y m + 1) b
  where placeStone' row = take (x m) row ++ [Just (stone m)] ++ drop (x m + 1) row
        x move = fst $ coord move 
        y move = snd $ coord move


-- | legalCoord checks if coordinates are in bound of the board array
-- >>> let b = board $ initBoard "(;SZ[4])"
-- >>> legalCoord b Move {stone = Black, coord = (-1,0)}
-- False
-- >>> legalCoord b Move {stone = Black, coord = (1,0)}
-- True
-- >>> legalCoord b Move {stone = Black, coord = (3,3)}
-- True
-- >>> legalCoord b Move {stone = Black, coord = (3,4)}
-- False
legalCoord :: Board -> Move -> Bool
legalCoord b (Move _ (x, y)) =
  let xrange = [0..(length b - 1)]
      yrange = [0..(length (head b) - 1)]
  in x `elem` xrange && y `elem` yrange


-- | removeIfNoLiberty removes stones from the board array if the stones
-- does not have any liberty, and returns the updated score
-- >>> let b1 = board $ initBoard "(;SZ[4];AB[aa][ab];AW[ba][bb][ac])"
-- >>> let b2 = board $ initBoard "(;SZ[4];AW[ba][bb][ac])"
-- >>> let m = [Move {stone = Black, coord = (0,0)}, Move {stone = Black, coord = (0,1)}]
-- >>> removeIfNoLiberty (5, b1) m == (7, b2)
-- True
removeIfNoLiberty :: (Int, Board) -> [Move] -> (Int, Board)
removeIfNoLiberty (score, b) m = if not $ hasLiberty m b then (score + length m, removeStones b m) else (score, b)


-- | capture
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


-- | suicide (self-capture) checks and apply rule to the game state
-- >>> let w1 = initBoard "(;SZ[13];AW[ba][ab][bc][cb];B[bb])"
-- >>> let w2 = initBoard "(;SZ[13];AW[ba][ab][bc][cb])"
-- >>> board w1 == board w2
-- True
suicide :: World -> Move -> World
suicide s m =
  let b = board s
      ownUnit = getUnit m b
      (num, board') = removeIfNoLiberty (0, b) ownUnit
  in s {players = addScore (players s) (opponent $ stone m) num, board = board'}
    
    
-- | addStoneToBoardAndRecord applies moves to the game state and appends the
-- move to the move list. The exception is in a ko fight
-- >>> let m = Move { stone = Black, coord = (0,1) }
-- >>> let w1 = addStoneToBoardAndRecord (initBoard "") m
-- >>> let w2 = (initBoard "(;SZ[13];AB[ab])") { moves = [m] }
-- >>> board w1 == board w2
-- True
-- >>> moves w1 == moves w2
-- True
-- >>> playerTurn w1 == White
-- True
addStoneToBoardAndRecord :: World -> Move -> World
addStoneToBoardAndRecord w m = if ko then w else newboard { prevBoard = board w, playerTurn = nextplayer }
  where b = board w
        newboard = suicide (capture (w {moves = moves w ++ [m], board = placeStone b m}) m) m
        ko = board newboard == prevBoard w
        nextplayer = opponent $ playerTurn w


-- | addStoneToBoardWithoutRecord applies move to the game state but does not append
-- the move to the move list. This is useful for processing prerecorded moves
-- from SGF
-- >>> let m = Move { stone = Black, coord = (0,1) }
-- >>> let w1 = addStoneToBoardWithoutRecord (initBoard "") m
-- >>> let w2 = (initBoard "(;SZ[13];AB[ab])") { moves = [m] }
-- >>> board w1 == board w2
-- True
-- >>> moves w1 == moves w2
-- False
-- >>> playerTurn w1 == White
-- False
addStoneToBoardWithoutRecord :: World -> Move -> World
addStoneToBoardWithoutRecord s m = suicide (capture (s { board = placeStone b m}) m) m
  where b = board s


-- | addScore increments the score of the player matching stone color
-- >>> let p1 = [Player {name = "W", captured = 0}, Player {name = "B", captured = 0}]
-- >>> let p2 = [Player {name = "W", captured = 5}, Player {name = "B", captured = 0}]
-- >>> addScore p1 White 5 == p2
-- True
addScore :: [Player] -> Stone -> Int -> [Player]
addScore [p1, p2] s n = if s == White then [add' p1, p2] else [p1, add' p2]
  where add' p = p { captured = captured p + n }
addScore x _ _ = x


-- | getUnit finds a list of adjacent stones of same color known as a group/unit
-- >>> let b = board $ initBoard "(;SZ[13];AB[cc][cd][dc])"
-- >>> let m = Move {stone = Black, coord = (2,2)}
-- >>> let exp = [Move {stone = Black, coord = (2,3)},Move {stone = Black, coord = (3,2)},Move {stone = Black, coord = (2,2)}]
-- >>> getUnit m b == exp
-- True
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


-- | hasLiberty checks if a unit have any liberties or not
-- >>> let b1 = board $ initBoard "(;SZ[13];AB[ba][bb][ac];AW[aa][ab])"
-- >>> let b2 = board $ initBoard "(;SZ[13];AB[ba][bb][ac];AW[aa])"
-- >>> let m = [Move {stone = White, coord = (0,0)}]
-- >>> hasLiberty m b1
-- False
-- >>> hasLiberty m b2
-- True
hasLiberty :: [Move] -> Board -> Bool
hasLiberty m b = any isNothing intersects
  where neighbors = concatMap (\x -> getNeighborCoords (coord x) b) m
        intersects = map (\x -> b !! snd x !! fst x) neighbors


-- | getNeighborCoords generates a list of adjacent coordinates. Useful for checking liberties
-- >>> let b = board $ initBoard "(;SZ[13];AW[ba][dd])"
-- >>> getNeighborCoords (1,0) b == [(0,0),(2,0),(1,1)]
-- True
getNeighborCoords :: Coord -> Board -> [Coord]
getNeighborCoords (x, y) b = filter (legalCoord b . Move Black) [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]


-- | opponent returns the opponent stone of any given stone
-- >>> opponent Black == White
-- True
-- >>> opponent White == Black
-- True
opponent :: Stone -> Stone
opponent s = if s == Black then White else Black

