{-# LANGUAGE OverloadedStrings #-}

module Game
    ( go
    ) where

import Board (initBoard)
import Graphics
import DataTypes
import SDL.Font
import System.IO (hFlush, stdout)
import qualified SDL
import Control.Exception

-- | go initiates the libraries and starts the entry point of the game
go :: IO ()
go = withSDL . withSDLImage . withSDLFont $ do
  setHintQuality
  withWindow "Go game" mainApp 



-- | Main entry to the game. It takes the handle to the SDL Window,
-- sets everything up and executes the main application loop: handle user inputs,
-- and draw the world.
mainApp :: SDL.Window -> IO ()
mainApp win =
    withRenderer win (\r -> do
      initWorld <- readFromFile "game.sgf"
      t <- traverse (loadTextureWithInfo r) ["./assets/wood.jpg", "./assets/whiteStone.png", "./assets/blackStone.png"]
      let texturedWorld = initWorld {boardTexture = head t, stoneTextureWhite = fst (t !! 1), stoneTextureBlack = fst (t !! 2)}
      let loop w = do
            events <- SDL.pollEvents
            let newWorld = updateWorld w events
            renderWorld r newWorld
            if exiting newWorld then return () else loop newWorld
      loop texturedWorld
      mapM_ (SDL.destroyTexture . fst) t
      )

readFromFile :: String -> IO World
readFromFile filename = do
      contents <- try (readFile filename) :: IO (Either IOException String)
      case contents of
        Left _ -> putStrLn ("could not read from file: " ++ filename) >> hFlush stdout >> return (initBoard "")
        Right str -> putStrLn ("loaded from file: " ++ filename) >> hFlush stdout >> return (initBoard str)
