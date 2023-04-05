module Graphics
    ( withSDL
    , withWindow
    , withSDLImage
    , withSDLFont
    , withRenderer
    , setHintQuality
    , updateWorld
    , renderWorld
    , loadTextureWithInfo
    ) where

{-# LANGUAGE OverloadedStrings #-}

import qualified SDL
import qualified SDL.Image
import qualified SDL.Font
import Control.Monad          (void, when)
import Control.Monad.IO.Class (MonadIO)
import Data.Text              (Text, pack)
import Data.Foldable          (foldl')
import SDL (($=))
import DataTypes
import Foreign.C.Types
import Data.Maybe (isJust, fromJust)
import Board

black :: SDL.Font.Color
black = SDL.V4 0 0 0 255

-- | setHintQuality sets nearest pixel sampling for scaling
setHintQuality :: (MonadIO m) => m ()
setHintQuality = SDL.HintRenderScaleQuality $= SDL.ScaleNearest

-- | withSDL initilizes the SDL library
withSDL :: (MonadIO m) => m a -> m ()
withSDL op = do
  SDL.initialize []
  void op
  SDL.quit

-- | withSDLImage initializes SDL Image library
withSDLImage :: (MonadIO m) => m a -> m ()
withSDLImage op = do
  SDL.Image.initialize []
  void op
  SDL.Image.quit

-- | withSDLFont initializes SDL Font library
withSDLFont :: (MonadIO m) => m a -> m ()
withSDLFont op = do
  SDL.Font.initialize
  void op
  SDL.Font.quit


-- | withWindow initializes SDL window and runs subsequent operations on the Window
withWindow :: (MonadIO m) => Text -> (SDL.Window -> m a) -> m ()
withWindow title op = do
  w <- SDL.createWindow title windowConfig
  SDL.showWindow w
  void $ op w
  SDL.destroyWindow w

-- | withRenderer sets up the renderer and uses it to execute rendering functions
withRenderer :: (MonadIO m) => SDL.Window -> (SDL.Renderer -> m a) -> m ()
withRenderer w op = do
  let config = SDL.RendererConfig
               { SDL.rendererType = SDL.AcceleratedVSyncRenderer
               , SDL.rendererTargetTexture = False
               }
  r <- SDL.createRenderer w (-1) config
  void $ op r
  SDL.destroyRenderer r


-- | loadTexdtureWithInfo loads textures from file path
loadTextureWithInfo :: (MonadIO m) => SDL.Renderer -> FilePath -> m (SDL.Texture, SDL.TextureInfo)
loadTextureWithInfo r p = do
  t <- SDL.Image.loadTexture r p
  i <- SDL.queryTexture t
  pure (t, i)


-- | updateWorld polls for events and ensures that they are applied
updateWorld :: World -> [SDL.Event] -> World
updateWorld w
  = foldl' (flip applyIntent) w
  . fmap (payloadToIntent . SDL.eventPayload)

-- | renderWorld is the render loop of the game
renderWorld :: SDL.Renderer -> World -> IO ()
renderWorld r w = do
  let pos = getEyeCoordFromMousePos w
  SDL.clear r
  drawBackground r w
  drawGrid r w
  drawStones r w
  drawScoreboard r w
  when (isJust pos) $ drawStone r w (fromJust pos) (playerTurn w)
  SDL.present r


-- | payloadToIntent converts event payload to intent 
payloadToIntent :: SDL.EventPayload -> Intent
payloadToIntent SDL.QuitEvent            = Quit -- window CLOSE pressed
payloadToIntent (SDL.KeyboardEvent e)    = -- When Q is pressed, quit also
  if SDL.keysymKeycode (SDL.keyboardEventKeysym e) == SDL.KeycodeQ then Quit else Idle
payloadToIntent (SDL.WindowResizedEvent e) = windowEventResize e
payloadToIntent (SDL.MouseMotionEvent e) = windowEventMouseMotion e
payloadToIntent (SDL.MouseButtonEvent e) = windowEventMouseButton e
payloadToIntent _                        = Idle

-- | windowEventMouseClick sets mouse press/release intent
windowEventMouseButton :: SDL.MouseButtonEventData -> Intent
windowEventMouseButton e = if SDL.mouseButtonEventMotion e == SDL.Pressed then MousePressed else MouseReleased

-- | windowEventResize sets window resize intent
windowEventResize :: SDL.WindowResizedEventData -> Intent
windowEventResize e = WindowResize $ fromIntegral <$> SDL.windowResizedEventSize e

-- | windowEventMouseMotion sets mouse motion intent with position
windowEventMouseMotion :: SDL.MouseMotionEventData -> Intent
windowEventMouseMotion e = MouseMovement $ fmap fromIntegral pos
  where (SDL.P pos) = SDL.mouseMotionEventPos e

-- | applyIntent updates the game state based on event/intent
applyIntent :: Intent -> World -> World
applyIntent (WindowResize c) w = w {windowSize = c, mousePos = mousePos initialWorld}
applyIntent (MouseMovement c) w = w {mousePos = c}
applyIntent MousePressed w = w
applyIntent MouseReleased w = eventMouseReleased w
applyIntent Idle w = w
applyIntent Quit w = w {exiting = True}

-- | eventMouseReleased applies mouse release event
eventMouseReleased :: World -> World
eventMouseReleased w = if isJust pos then apply else w
  where pos = getEyeCoordFromMousePos w
        move = Move {stone = playerTurn w, coord = fromJust pos}
        apply = addStoneToBoardAndRecord w move


-- | drawBackground draws a repeated background texture
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
              SDL.copy r t Nothing (Just $ makeRect (SDL.V2 x y, SDL.V2 texWidth texHeight))
              loop (x + texWidth) y
  loop 0 0

-- | drawGrid draws the game board to the renderer
drawGrid :: SDL.Renderer -> World -> IO ()
drawGrid r w = do
  mapM_ (uncurry (SDL.drawLine r)) xlines
  mapM_ (uncurry (SDL.drawLine r)) ylines
  where max' = size w - 1
        xlines = [(coordToP (0, y), coordToP (max', y)) | y <- [0..max']]
        ylines = [(coordToP (y, 0), coordToP (y, max')) | y <- [0..max']]
        coordToP pos = SDL.P $ getAbsPos pos w

-- | drawStones draws the stone texture for each stone at each coordinate to the renderer
drawStones :: SDL.Renderer -> World -> IO ()
drawStones r w = mapM_ (\x -> drawStone r w (fst x) (fromJust $ snd x)) stoneList
  where stoneList :: [(Coord, Maybe Stone)]
        stoneList = [((j, i), value) | (i, col) <- zip [0..] (board w), (j, value) <- zip [0..] col, isJust value]

-- | drawStone draws a single stone texture based on coordinate
drawStone :: SDL.Renderer -> World -> Coord -> Stone -> IO ()
drawStone r w c s = SDL.copy r texture Nothing rect
  where rect = Just $ makeRect $ positionStone c w
        texture = selectTexture s
        selectTexture :: Stone -> SDL.Texture
        selectTexture s'
          | s' == White = stoneTextureWhite w
          | otherwise = stoneTextureBlack w

-- | positionStone offsets the abs texture coordinates for a stone, based on size
positionStone :: Coord -> World -> (SDL.V2 CInt, SDL.V2 CInt)
positionStone c w = (getAbsPos c w - SDL.V2 stoneRadius stoneRadius, SDL.V2 stoneSize stoneSize)
  where stoneSize = getStoneSize w
        stoneRadius = getStoneRadius w

-- | getStoneSize calculates how large the stone should be drawn based on the window scaling
-- >>> let w = initialWorld { windowSize = SDL.V2 1024 600 }
-- >>> getStoneSize w
-- 36
getStoneSize :: World -> CInt
getStoneSize w = truncate $ fromIntegral (getGridStepSize w) * (0.8 :: Double)

-- | getStoneRadius calculates the radius each stone should have
-- >>> let w = initialWorld { windowSize = SDL.V2 1024 600 }
-- >>> getStoneRadius w
-- 18
getStoneRadius :: World -> CInt
getStoneRadius w = truncate $ fromIntegral (getStoneSize w) / (2.0 :: Double)

-- | getEyeCoordFromMousePos calculates which intersection the mouse hovers over
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


-- | makeRect returns a rectangle based on the vector positions provided
makeRect :: (SDL.V2 a, SDL.V2 a) -> SDL.Rectangle a
makeRect (start, end) = SDL.Rectangle (SDL.P start) end


-- | getAbsPos
-- >>> getAbsPos (0,0) $ initialWorld { windowSize = SDL.V2 1024 1024, boardAreaStart = SDL.V2 0.1 0.1, size = 19 }
-- V2 102 102
getAbsPos :: Coord -> World -> AbsPos
getAbsPos (x, y) w = SDL.V2 (fromIntegral x * step) (fromIntegral y * step) + start
  where step = getGridStepSize w
        start = getGridStartPos w


-- | getGridStepSize
-- >>> getGridStepSize $ initialWorld { windowSize = SDL.V2 1024 1024, boardAreaStart = SDL.V2 0.1 0.1, size = 19 }
-- 28
getGridStepSize :: World -> CInt
getGridStepSize w = truncate $ min width height / fromIntegral (size w - 1)
  where (SDL.V2 width height) = cintToCFloat (windowSize w) * (boardAreaEnd w - boardAreaStart w)


-- | getGridStartPos
-- >>> getGridStartPos $ initialWorld { windowSize = SDL.V2 1024 1024, boardAreaStart = SDL.V2 0.1 0.1, size = 19 }
-- V2 102 102
getGridStartPos :: World -> AbsPos
getGridStartPos w = cfloatToCInt $ cintToCFloat (windowSize w) * boardAreaStart w


-- | cfloatToInt converts 2V CFloat to V2 CInt
-- >>> cfloatToCInt (SDL.V2 5.5 2.9) == (SDL.V2 5 2)
-- True
cfloatToCInt :: SDL.V2 CFloat -> SDL.V2 CInt
cfloatToCInt (SDL.V2 a b) = SDL.V2 (truncate a) (truncate b)


-- | cintToCFloat converts V2 CInt to V2 CFloat
-- >>> cintToCFloat (SDL.V2 5 2) == (SDL.V2 5.0 2.0)
-- True
cintToCFloat :: SDL.V2 CInt -> SDL.V2 CFloat
cintToCFloat (SDL.V2 a b) = SDL.V2 (fromIntegral a :: CFloat) (fromIntegral b :: CFloat)


-- | drawText draws text to the renderer
-- the font is loaded from memory every time because scaling a fixed font size looks terrible
-- this function could be optimized but it will require a lot more work which is not a requirement
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



-- | drawScoreboard writes the names and num of captured stones for each player to the renderer
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
              let scale@(SDL.V2 x _) = cintToCFloat $ squareMinimum areaSize
              let areaStart = getGridStartPos w + SDL.V2 (getGridStepSize w * fromIntegral (size w)) 0
              let absPos = areaStart + cfloatToCInt (p * scale)
              let fontsize = truncate $ relSize * realToFrac (x / 15.0)
              drawText r (pack s) absPos fontsize

            -- | getAreaSize returns the size of an area based on percentages
            getAreaSize :: World -> RelativePos -> RelativePos -> AbsPos
            getAreaSize w' a b = cfloatToCInt $ (b - a) * cintToCFloat (windowSize w')

            -- | squareMinimum squares the smallest side of a rectangle
            squareMinimum :: Ord a => SDL.V2 a -> SDL.V2 a
            squareMinimum (SDL.V2 n m) = let min' = min n m in SDL.V2 min' min'


