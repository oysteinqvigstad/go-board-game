module Text (parser
            , strToPos
            ) where

import DataTypes
import Data.Char (toLower, ord)

-- | parser is the entrypoint for converting a SGF string to game state
parser :: String -> World
parser [] = initialWorld
parser xs = readGameTree initialWorld xs

-- | readGameTree calls readSequence for any game trees found in the string
readGameTree :: World -> [Char] -> World
readGameTree state (x:xs)
  | x == '(' = readSequence state xs
  | otherwise = readGameTree state xs
readGameTree state _ = state

-- | readSequence calls readProperty for all properties found in the sequence
readSequence :: World -> [Char] -> World
readSequence state (x:xs)
  | x == ';' = let (newWorld, newString) = readProperty state xs in readSequence newWorld newString
  | otherwise = readSequence state xs
readSequence state _ = state

-- | readProperty splits each property into a key value pair and calls setProp for each
readProperty :: World -> String -> (World, String)
readProperty w as@(x:xs)
  | x `elem` [';', ')', '('] = (w, as)
  | x `elem` ['A'..'z'] = let (prop, _, _) = splitProp as in uncurry readProperty $ addProp w prop as
  | otherwise = readProperty w xs
  where addProp :: World -> String -> String -> (World, String)  
        addProp w2 prop str = if multiple then addProp (fst ret) prop (snd ret) else ret 
          where (_, value, substr) = splitProp str
                multiple = not (null substr) && head substr == '['
                ret = (setProp w2 (prop, value), substr)
readProperty state _ = (state, "")


-- | setProp sets the game state based on key value pair provided
setProp :: World -> (String, String) -> World
setProp state (key, value)
  | key == "PW" = let p = players state in state { players = [(head p) {name = value}, last p]}
  | key == "PB" = let p = players state in state { players = [head p, (last p) {name = value}]}
  | key == "SZ" = state { size = read value }
  | key == "B" = state { moves = moves state ++ [Move {stone = Black, coord = strToPos value}]}
  | key == "AB" = state { setup = setup state ++ [Move {stone = Black, coord = strToPos value}]}
  | key == "W" = state { moves = moves state ++ [Move {stone = White, coord = strToPos value}]}
  | key == "AW" = state { setup = setup state ++ [Move {stone = White, coord = strToPos value}]}
  | otherwise = state
  
  
-- | splitProp splits the string into (key, value, substring)
splitProp :: String -> (String, String, String)
splitProp xs =
  let (key, substr) = break (=='[') xs
      (value, leftover) = break (==']') substr
  in (key, tail value, tail leftover)

-- | strToPos converts a coordinate from chars to integers
strToPos :: String -> Coord
strToPos xs = let n = map (flip (-)97 . ord . toLower) xs in (head n, last n)
