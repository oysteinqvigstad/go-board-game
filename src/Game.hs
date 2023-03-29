{-# LANGUAGE OverloadedStrings #-}

module Game
    ( go
    ) where

import Board (initBoard)
import Graphics
import DataTypes
import SDL.Font
import qualified SDL


go :: IO ()
go = withSDL . withSDLImage . withSDLFont $ do
  setHintQuality
  withWindow "Go game" mainApp 



-- Main entry to our application logic. It takes the handle to the SDL Window,
-- sets everything up and executes the main application loop: handle user inputs,
-- and draw the world.
mainApp :: SDL.Window -> IO ()
mainApp win =
    withRenderer win (\r -> do
      let initWorld = initBoard "(;GM[1]FF[4]CA[UTF-8]AP[CGoban:3]ST[2]RU[Japanese]SZ[13]KM[0.00]PW[Walter White]PB[Jack Black];AW[bb][bc][cb]AB[ba][ab][db][ac][bd][ca]AW[fa]"
      t <- traverse (loadTextureWithInfo r) ["./assets/wood.jpg", "./assets/whiteStone.png", "./assets/blackStone.png"]
      font <- SDL.Font.load "./ttf/roboto/Roboto-Regular.ttf" 70
      let texturedWorld = initWorld {boardTexture = head t, stoneTextureWhite = fst (t !! 1), stoneTextureBlack = fst (t !! 2)}
      let loop w = do
            events <- SDL.pollEvents
            let newWorld = updateWorld w events
            renderWorld r newWorld
            if exiting newWorld then return () else loop newWorld
      loop texturedWorld
      mapM_ (SDL.destroyTexture . fst) t
      )
      
      


-- gameLoop is where the game is updated
--gameLoop :: GameData -> Stone -> IO ()
--gameLoop gameData@(_, board) s = do
--  printBoard board
--  move <- readNextMove (length board - 1) s
--  if isOccupied board move
--    then do
--      putStrLn "illegal move"
--      gameLoop gameData s
--  else do
--      gameLoop (addStoneToBoardAndRecord gameData move) $ opponent s



-- readNextMove reads input from user and returns the move to the game loop
--readNextMove :: Int -> Stone -> IO Move
--readNextMove max' s = do
--  putStr $ "Next move for '" ++ [stoneToChar $ Just s] ++ "' (e.g. 'aa'): "
--  hFlush stdout
--  pos <- getLine
--  let move = getMove pos max' s
--  if isJust move
--    then do
--      return $ fromJust move
--    else do
--      putStrLn "Not legal input"
--      readNextMove max' s


--getMove :: String -> Int -> Stone -> Maybe Move
--getMove xs max' s
--  | length xs /= 2 = Nothing
--  | all (\x -> x `elem` [0..max']) numbers = Just $ Move s (head numbers, last numbers)
--  | otherwise = Nothing
--  where numbers = map (flip (-)97 . ord . toLower) xs -- TODO: Replace with function below



