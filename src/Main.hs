module Main where

import Colours

import Linear.V2
import System.Exit

import Helm
import Helm.Engine.SDL
import Helm.Graphics2D
import Helm.Graphics2D.Text
import Helm.Sub (none)
import Helm.Window

import qualified Helm.Cmd as Cmd
import qualified Helm.Sub as Sub
import qualified Helm.Time as Time
import qualified Helm.Keyboard as Key

data Model = Model
  -- The width and height of the Helm window
  { window  :: V2 Int
  -- The radius of each circle on the screen
  , circles :: [Double]
  -- (Angle on outer circle, velocity)
  , player :: (Double, Double)
  }

-- Clockwise | Anticlockwise
data Direction = Forward | Backward deriving Eq

-- Possible actions are to create a new circle or do nothing
data Action = Tick
            | NewCircle 
            | Move Direction 
            | Resize Int Int
            | Quit
            | Nil 
            deriving Eq

config :: GameConfig SDLEngine Model Action
config = GameConfig
 { initialFn = init
 , updateFn  = update
 , viewFn    = view
 , subscriptionsFn = sub
 }
 where
 -- Initial configuration has the default (800x600) window and one circle
 init :: (Model, Cmd SDLEngine Action)
 init = (model, Cmd.none)
  where
  model = Model { window  = (windowDimensions defaultConfig)
                , circles = [0.0]
                , player  = (pi / 2, 0)
                }

 -- We want to add circles when commanded to, and expand existing ones
 update :: Model -> Action -> (Model, Cmd SDLEngine Action)
 update m NewCircle = (m { circles = 0.0:(circles m) }, Cmd.none)
 update m (Move d) = ((m { player = setSpeed (player m) d }), Cmd.none)
  where
  setSpeed :: (Double, Double) -> Direction -> (Double, Double)
  setSpeed (x, _) Forward  = (x,  0.05)
  setSpeed (x, _) Backward = (x, -0.05)

 update m Quit = (m, Cmd.execute exitSuccess (\_ -> Quit))
 update m Tick = (m { window = (window m)
                    , circles = concatMap expandOrDel (circles m)
                    , player  = let (x,v) = (player m) in (x + v, v)
                    }
                 , Cmd.none)
  where
  -- Expand a circle's radius by 5 unless it's no longer visible
  expandOrDel :: Double -> [Double]
  expandOrDel n = if (n >= cutoff (window m)) then [] else [n + 5]

  -- Determine the cutoff radius for a circle being visible, depending on
  -- window dimensions
  cutoff :: V2 Int -> Double
  cutoff (V2 x y) = sqrt ((fromIntegral (x*x + y*y)) / 4.0)

 update m (Resize w h) = (m { window = V2 w h }, Cmd.none)
 update m a = (m, Cmd.none)

 
 -- Subscribe to update at 60FPS and create a new circle every second
 sub :: Sub SDLEngine Action
 sub = Sub.batch [Time.fps 60 (\t -> Tick)
                 ,Time.every Time.second (\t -> NewCircle)
                 ,Key.downs (\k -> case k of
                                         Key.LeftKey  -> Move Backward
                                         Key.RightKey -> Move Forward
                                         _            -> Nil)
                 ,Key.presses (\k -> if k == Key.QKey then Quit else Nil)
                 ,resizes (\(V2 w h) -> Resize w h)
                 ]

 view :: Model -> Graphics SDLEngine
 view m = Graphics2D $ center origin screen
  where
  -- Smallest window dimension - used for the radius of playerCircle
  cutoff = let (w,h) = (\(V2 x y) -> (x,y)) (window m)
           in  (fromIntegral $ (min w h) - 50) / 2

  -- Combination of all of the elements for display
  screen :: Collage SDLEngine
  screen = collage $ playerCircle:map mkCircle (circles m) ++ [player']

  -- Turn a radius into a green circle. Yellow if it's past playerCircle
  mkCircle :: Double -> Form SDLEngine
  mkCircle r
    | r >= cutoff = outlined (solid yellow) (circle r)
    | otherwise   = outlined (solid green) (circle r)

  -- The outer circle on which the player resides
  playerCircle = outlined thickBlueLine (circle cutoff)

  -- The dot representing the player
  player' = move playerPosition $ filled red (circle 10)

  -- Angle on playerCircle -> V2 X Y
  playerPosition :: V2 Double
  playerPosition = (pure cutoff) * V2 (cos angle) (sin angle)

  -- The angle of the player on the playerCircle
  angle = fst $ player m

  -- The middle of the window to center the collage at
  origin = (fromIntegral <$> (window m)) / (V2 2.0 2.0)

  -- A thicker (blue) LineStyle for the static player circle
  thickBlueLine :: LineStyle
  thickBlueLine = defaultLine { lineColor = blue
                              , lineWidth = 4
                              }

main :: IO ()
main = do
 engine <- startup
 run engine config
