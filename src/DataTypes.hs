module DataTypes where
import qualified SDL
import Foreign.C.Types
import qualified SDL.Font
  
data Stone = Black | White deriving (Show, Eq)
type Board = [[Maybe Stone]]

type Coord = (Int, Int)
type RelativePos = SDL.V2 CFloat
type AbsPos = SDL.V2 CInt
type Grid = [SDL.Rectangle CInt]

data Move = Move { stone :: Stone
                 , coord :: Coord } deriving (Show, Eq)
                 
data Player = Player { name :: String
                     , captured :: Int } deriving (Show)
                 
data World = World
  { exiting :: Bool
  , playerTurn :: Stone
  , windowSize :: AbsPos
  , boardAreaStart :: RelativePos
  , boardAreaEnd :: RelativePos
  , scoreboardAreaStart :: RelativePos
  , scoreboardAreaEnd :: RelativePos
  , boardTexture :: (SDL.Texture, SDL.TextureInfo)
  , stoneTextureBlack :: SDL.Texture
  , stoneTextureWhite :: SDL.Texture
  , mousePos :: AbsPos
  , players :: [Player]
  , size :: Int
  , setup :: [Move]
  , moves :: [Move] 
  , board :: Board
  } 
  
data Intent
  = Idle
  | WindowResize AbsPos
  | MouseMovement AbsPos
  | MousePressed
  | MouseReleased
  | Quit



initialPlayer :: Player
initialPlayer = Player { name = ""
                       , captured = 0
                       } 

initialWorld :: World
initialWorld = World { exiting = False
                     , playerTurn = Black
                     , windowSize = SDL.V2 1024 600
                     , boardAreaStart = SDL.V2 0.05 0.05
                     , boardAreaEnd = SDL.V2 0.6 0.95
                     , scoreboardAreaStart = SDL.V2 0.6 0.05
                     , scoreboardAreaEnd = SDL.V2 0.95 0.95
                     , boardTexture = undefined
                     , stoneTextureBlack = undefined
                     , stoneTextureWhite = undefined
                     , mousePos = SDL.V2 (-100) (-100)
                     , players = [initialPlayer, initialPlayer]
                     , size = 0
                     , setup = []
                     , moves = []
                     , board = [[]]
                     }                
                     
windowConfig :: SDL.WindowConfig
windowConfig = SDL.defaultWindow { SDL.windowInitialSize = windowSize initialWorld
                                  , SDL.windowResizable = True
                                  }          