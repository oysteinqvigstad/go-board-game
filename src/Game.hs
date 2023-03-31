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
      let texturedWorld = initWorld {boardTexture = head t, stoneTextureWhite = fst (t !! 1), stoneTextureBlack = fst (t !! 2)}
      let loop w = do
            events <- SDL.pollEvents
            let newWorld = updateWorld w events
            renderWorld r newWorld
            if exiting newWorld then return () else loop newWorld
      loop texturedWorld
      mapM_ (SDL.destroyTexture . fst) t
      )