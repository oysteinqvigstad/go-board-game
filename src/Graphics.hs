{-# LANGUAGE OverloadedStrings #-}

module Graphics where

import qualified SDL 
import qualified SDL.Image
import qualified SDL.Font
import Control.Monad          (void)
import Control.Monad.IO.Class (MonadIO)
import Data.Text              (Text, pack)
import Data.Foldable          (foldl')
import SDL (($=))
import DataTypes
import Foreign.C.Types
import Data.Maybe (isJust, fromJust, isNothing)
import Board

black :: SDL.Font.Color
black = SDL.V4 0 0 0 255

-- Sets SDL.HintRenderScaleQuality
setHintQuality :: (MonadIO m) => m ()
setHintQuality = SDL.HintRenderScaleQuality $= SDL.ScaleNearest

-- Sets SDL library and executes an operation in SDL context.
-- Clears SDL when operation finishes.
withSDL :: (MonadIO m) => m a -> m ()
withSDL op = do
  SDL.initialize []
  void op
  SDL.quit

-- Sets SDL.Image library and executes any given operation in the context.
withSDLImage :: (MonadIO m) => m a -> m ()
withSDLImage op = do
  SDL.Image.initialize []
  void op
  SDL.Image.quit

withSDLFont :: (MonadIO m) => m a -> m ()
withSDLFont op = do
  SDL.Font.initialize
  void op
  SDL.Font.quit


-- Given title and dimensions it sets SDL Window and uses it to
-- run subsequent operations on the Window. Clears when done.
withWindow :: (MonadIO m) => Text -> (SDL.Window -> m a) -> m ()
withWindow title op = do
  w <- SDL.createWindow title windowConfig
  SDL.showWindow w
  void $ op w
  SDL.destroyWindow w

-- Given SDL window, it sets up render and uses it to execute rendering function,
-- clears renderer when done.
withRenderer :: (MonadIO m) => SDL.Window -> (SDL.Renderer -> m a) -> m ()
withRenderer w op = do
  let config = SDL.RendererConfig
               { SDL.rendererType = SDL.AcceleratedVSyncRenderer
               , SDL.rendererTargetTexture = False
               }
  r <- SDL.createRenderer w (-1) config
  void $ op r
  SDL.destroyRenderer r


-- Loads the texture with info as a tuple.
loadTextureWithInfo :: (MonadIO m) => SDL.Renderer -> FilePath -> m (SDL.Texture, SDL.TextureInfo)
loadTextureWithInfo r p = do
  t <- SDL.Image.loadTexture r p
  i <- SDL.queryTexture t
  pure (t, i)




-- Given a list of events, update the world
updateWorld :: World -> [SDL.Event] -> World
updateWorld w
  = foldl' (flip applyIntent) w
  . fmap (payloadToIntent . SDL.eventPayload)


renderWorld :: SDL.Renderer -> World -> IO ()
renderWorld r w = do
  let pos = getEyeCoordFromMousePos w
  SDL.clear r
  drawBackground r w
  drawGrid r w
  drawStones r w
  drawScoreboard r w
  if isJust pos
    then do
      drawStone r w (fromJust pos) (playerTurn w)
    else
      return ()
  SDL.present r


-- Convert the SDL event to Intent
payloadToIntent :: SDL.EventPayload -> Intent
payloadToIntent SDL.QuitEvent            = Quit -- window CLOSE pressed
payloadToIntent (SDL.KeyboardEvent e)    = -- When Q is pressed, quit also
  if SDL.keysymKeycode (SDL.keyboardEventKeysym e) == SDL.KeycodeQ then Quit else Idle
payloadToIntent (SDL.WindowResizedEvent e) = windowEventResize e
payloadToIntent (SDL.MouseMotionEvent e) = windowEventMouseMotion e
payloadToIntent (SDL.MouseButtonEvent e) = windowEventMouseClick e
payloadToIntent _                        = Idle


windowEventMouseClick :: SDL.MouseButtonEventData -> Intent
windowEventMouseClick e = if SDL.mouseButtonEventMotion e == SDL.Pressed then MousePressed else MouseReleased


windowEventResize :: SDL.WindowResizedEventData -> Intent
windowEventResize e = WindowResize size
  where size = fromIntegral <$> SDL.windowResizedEventSize e


windowEventMouseMotion :: SDL.MouseMotionEventData -> Intent
windowEventMouseMotion e = MouseMovement $ fmap fromIntegral pos
  where (SDL.P pos) = SDL.mouseMotionEventPos e

applyIntent :: Intent -> World -> World
applyIntent (WindowResize c) w = w {windowSize = c, mousePos = mousePos initialWorld}
applyIntent (MouseMovement c) w = w {mousePos = c}
applyIntent MousePressed w = w
applyIntent MouseReleased w = eventMouseReleased w
applyIntent Idle w = w
applyIntent Quit w = w {exiting = True}

eventMouseReleased :: World -> World
eventMouseReleased w = if isJust pos then apply {playerTurn = nextplayer} else w
  where pos = getEyeCoordFromMousePos w
        move = Move {stone = playerTurn w, coord = fromJust pos}
        apply = addStoneToBoardAndRecord w move
        nextplayer = opponent $ playerTurn w


-- The actual method for drawing that is used by the rendering method above.
drawBackground :: SDL.Renderer -> World -> IO ()
drawBackground r w = do
  let (SDL.V2 winWidth winHeight) = windowSize w
  let (t, ti) = boardTexture w
  let texHeight = SDL.textureHeight ti `div` 2
  let texWidth = SDL.textureWidth ti `div` 2
  let loop x y
          | y >= winHeight = return ()
          | x >= winWidth = loop 0 (y + texHeight)
          | otherwise = do
              SDL.copy r t Nothing (Just $ mkRect x y texWidth texHeight)
              loop (x + texWidth) y
  loop 0 0

-- Makes SDL rectangle out of x y width and height
mkRect :: a -> a -> a -> a-> SDL.Rectangle a
mkRect x y w h = SDL.Rectangle o z
  where
    o = SDL.P (SDL.V2 x y)
    z = SDL.V2 w h


drawGrid :: SDL.Renderer -> World -> IO ()
drawGrid r w = do
  mapM_ (uncurry (SDL.drawLine r)) xlines
  mapM_ (uncurry (SDL.drawLine r)) ylines
  where max' = size w - 1
        xlines = [(coordToP (0, y), coordToP (max', y)) | y <- [0..max']]
        ylines = [(coordToP (y, 0), coordToP (y, max')) | y <- [0..max']]
        coordToP pos = SDL.P $ getAbsPos pos w


drawStones :: SDL.Renderer -> World -> IO ()
drawStones r w = mapM_ (\x -> drawStone r w (fst x) (fromJust $ snd x)) stoneList
  where stoneList :: [(Coord, Maybe Stone)]
        stoneList = [((j, i), value) | (i, col) <- zip [0..] (board w), (j, value) <- zip [0..] col, isJust value]


drawStone :: SDL.Renderer -> World -> Coord -> Stone -> IO ()
drawStone r w c stone = SDL.copy r texture Nothing rect
  where rect = Just $ makeRect $ positionStone c w
        texture = selectTexture stone
        selectTexture :: Stone -> SDL.Texture
        selectTexture s
          | s == White = stoneTextureWhite w
          | otherwise = stoneTextureBlack w


positionStone :: Coord -> World -> (SDL.V2 CInt, SDL.V2 CInt)
positionStone c w = (getAbsPos c w - SDL.V2 stoneRadius stoneRadius, SDL.V2 stoneSize stoneSize)
  where stoneSize = getStoneSize w
        stoneRadius = getStoneRadius w


getStoneSize :: World -> CInt
getStoneSize w = truncate $ fromIntegral (getGridStepSize w) * 0.8


getStoneRadius :: World -> CInt
getStoneRadius w = truncate $ fromIntegral (getStoneSize w) / 2.0


getEyeCoordFromMousePos :: World -> Maybe Coord
getEyeCoordFromMousePos w
        | x < 0 || x >= size w = Nothing
        | y < 0 || y >= size w = Nothing
        | isJust (board w !! y !! x) = Nothing
        | otherwise = Just (x, y)
        where mouseC = cintToCFloat $ mousePos w
              step = getGridStepSize w
              gridStart = boardAreaStart w * cintToCFloat (windowSize w)
              step' = cintToCFloat $ SDL.V2 step step
              (SDL.V2 x' y') = cfloatToCInt $ (mouseC - gridStart + SDL.V2 0.5 0.5 * step') / step'
              (x, y) = (fromIntegral x', fromIntegral y')


-- makeRect returns a rectangle based on the vector positions provided
makeRect :: (SDL.V2 CInt, SDL.V2 CInt) -> SDL.Rectangle CInt
makeRect (start, end) = SDL.Rectangle (SDL.P start) end


-- | getAbsPos'
-- >>> getAbsPos' (0,0) $ initialWorld { windowSize = SDL.V2 1024 1024, boardAreaStart = SDL.V2 0.1 0.1, size = 19 }
getAbsPos :: Coord -> World -> AbsPos
getAbsPos (x, y) w = SDL.V2 (fromIntegral x * step) (fromIntegral y * step) + start
  where step = getGridStepSize w
        start = getGridStartPos w


-- | getGridStepSize
-- >>> getGridStepSize $ initialWorld { windowSize = SDL.V2 1024 1024, boardAreaStart = SDL.V2 0.1 0.1, size = 19 }
--
getGridStepSize :: World -> CInt
getGridStepSize w = truncate $ min width height / fromIntegral (size w - 1)
  where (SDL.V2 width height) = cintToCFloat (windowSize w) * (boardAreaEnd w - boardAreaStart w)


-- | getGridStartPos
-- >>> getGridStartPos $ initialWorld { windowSize = SDL.V2 1024 1024, boardAreaStart = SDL.V2 0.1 0.1, size = 19 }
-- V2 102 102
getGridStartPos :: World -> AbsPos
getGridStartPos w = cfloatToCInt $ cintToCFloat (windowSize w) * boardAreaStart w


-- returns the size in pixels (x, y) of the window context provided
getWindowSize :: SDL.Window -> IO (Int, Int)
getWindowSize w = do
  (SDL.V2 width height) <- fmap fromIntegral <$> SDL.get (SDL.windowSize w)
  return (width, height)
  
-- converts 2V CFloat to V2 CInt
cfloatToCInt :: SDL.V2 CFloat -> SDL.V2 CInt
cfloatToCInt (SDL.V2 a b) = SDL.V2 (truncate a) (truncate b)

-- converts V2 CInt to V2 CFloat
cintToCFloat :: SDL.V2 CInt -> SDL.V2 CFloat
cintToCFloat (SDL.V2 a b) = SDL.V2 (fromIntegral a :: CFloat) (fromIntegral b :: CFloat)


-- | Let us draw some text
drawText :: SDL.Renderer -> Text -> AbsPos -> Int -> IO ()
drawText r t pos scale = do
        font <- SDL.Font.load "./ttf/roboto/Roboto-Regular.ttf" scale
        textSurface <- SDL.Font.blended font black t
        SDL.Font.free font
        surface <- SDL.createTextureFromSurface r textSurface
        info <- SDL.queryTexture surface
        let width = SDL.textureWidth info
        let height = SDL.textureHeight info
        SDL.copy r surface Nothing (Just $ SDL.Rectangle (SDL.P pos) (SDL.V2 width height))
        SDL.freeSurface textSurface




drawScoreboard :: SDL.Renderer -> World -> IO ()
drawScoreboard r w = do
      let p1 = head . players $ w
      let p2 = last . players $ w
      put (name p1) (SDL.V2 0.0 0.0) 2.0
      put ("Captured: " ++ show (captured p1)) (SDL.V2 0.0 0.15) 1.0
      put (name p2) (SDL.V2 0.0 0.3) 2.0
      put ("Captured: " ++ show (captured p2)) (SDL.V2 0.0 0.45) 1.0
      where put :: String -> RelativePos -> Float -> IO ()
            put s p relSize = do
              let areaSize = getAreaSize w (scoreboardAreaStart w) (scoreboardAreaEnd w)
              let scale@(SDL.V2 x _) = cintToCFloat $ squareMinimumV2 areaSize
              let areaStart = getGridStartPos w + SDL.V2 (getGridStepSize w * fromIntegral (size w)) 0
              let absPos = areaStart + cfloatToCInt (p * scale)
              let fontsize = truncate $ relSize * realToFrac (x / 15.0)
              drawText r (pack s) absPos fontsize


getAreaSize :: World -> RelativePos -> RelativePos -> AbsPos
getAreaSize w a b = cfloatToCInt $ (b - a) * cintToCFloat (windowSize w)

squareMinimumV2 :: Ord a => SDL.V2 a -> SDL.V2 a
squareMinimumV2 (SDL.V2 n m) = let min' = min n m in SDL.V2 min' min'


