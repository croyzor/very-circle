module Main where

-- Local modules
import Colours
import Drawing
import Types
import qualified Config as Cfg

-- Miscellaneous imports
import Data.Maybe
import Linear.V2
import System.Exit
import System.Random

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

-- Initial configuration has the default (800x600) window and one circle
-- mkInit takes an RNG and adds it to the model it returns
mkInit :: StdGen -> (Model, Cmd SDLEngine Action)
mkInit g = (model, Cmd.none)
 where
 model = Model { window  = (windowDimensions defaultConfig)
               , circles = [(0.0, Nothing)]
               , player  = (pi / 2, 0)
               , gen     = g
               }

config :: GameConfig SDLEngine Model Action
config = GameConfig
 -- Before we attempt to get the global RNG (and if it fails) mkStdGen
 -- provides us with a predictable RNG without using IO
 { initialFn = mkInit (mkStdGen 1)
 , updateFn  = update
 , viewFn    = view
 , subscriptionsFn = sub
 }
 where
 update :: Model -> Action -> (Model, Cmd SDLEngine Action)

 -- Add a new circle (and obstacle) to the game
 update m NewCircle = (m { circles = circles'
                         , gen = g'
                         }
                      , Cmd.none)
  where
  -- Generate random position and velocity for the obstacle
  (x,g) = randomR (0.0, 6.28) (gen m) :: (Double, StdGen)
  (v,g') = randomR (-0.2, 0.2) g :: (Double, StdGen)
  circles' = (0.0, Just (x, v)) : (circles m)

 -- Handle player movement - rotate player either left or right
 update m (Move d) = ((m { player = setSpeed (player m) d }), Cmd.none)
  where
  setSpeed :: (Angle, Velocity) -> Direction -> (Angle, Velocity)
  setSpeed (x, _) Forward  = (x,  0.05)
  setSpeed (x, _) Backward = (x, -0.05)

 -- Every frame, call updateCircle (on every Circle) and move the player
 update m Tick = (m { circles = concatMap updateCircle (circles m)
                    , player  = let (x,v) = (player m) in (x + v, v)
                    }
                 , Cmd.none)
  where
  -- Expand a circle's radius by 5 unless it's no longer visible
  expandOrDel :: Radius -> [Radius]
  expandOrDel n = if (n >= cutoff (window m)) then [] else [n + 5]

  -- Expand a circle and increase the velocity of it's obstacle
  -- (return a list so that we can concatenate the results)
  updateCircle :: Circle -> [Circle]
  updateCircle (r, Nothing) = (expandOrDel r) >>= \x -> [(x, Nothing)]
  updateCircle (r, Just (x,v)) = (expandOrDel r) >>= \z -> [(z, Just (x+v,v))]

  -- Determine the cutoff radius for a circle being visible, depending on
  -- window dimensions
  cutoff :: V2 Int -> Radius
  cutoff (V2 x y) = sqrt ((fromIntegral (x*x + y*y)) / 4.0)

 -- Handle window resize by modifying the window dimensions in the model
 update m (Resize w h) = (m { window = V2 w h }, Cmd.none)

 -- Exit game by killing this process (and announcing success to the OS)
 update m Quit = (m, Cmd.execute exitSuccess (\_ -> Quit))
 update m a = (m, Cmd.none)

 {- Update at framerate defined in Config
  - Create a new circle every second
  - Handle left and right keys to change player direction
  - Handle `Q` to quit
  - Tell model to handle resize events -}
 sub :: Sub SDLEngine Action
 sub = Sub.batch [Time.fps Cfg.fps (\t -> Tick)
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
  screen = collage $ playerCircle : map (mkCircle.fst) (circles m)
                   ++ [player'] ++ map (\z ->
                      case z of
                        (r,Just (x,_)) -> drawObstacle r x
                        (r,Nothing)    -> blank) (circles m)

  -- Turn a radius into a green circle. Yellow if it's past playerCircle
  mkCircle :: Radius -> Form SDLEngine
  mkCircle r
    | r >= cutoff = outlined (solid yellow) (circle r)
    | otherwise   = outlined (solid green) (circle r)

  -- The outer circle on which the player resides
  playerCircle = outlined thickBlueLine (circle cutoff)

  -- The object representing the player
  player' = moveCartesian (cutoff, angle) redCircle

  -- Draw a wee circle from polar coordinates (Radius, Angle)
  drawObstacle :: Double -> Double -> Form SDLEngine
  drawObstacle r a = moveCartesian (r,a) redCircle

  -- The angle of the player on the playerCircle
  angle = fst $ player m

  -- The middle of the window to center the collage at
  origin = (fromIntegral <$> (window m)) / (pure 2.0)


main :: IO ()
main = do
 -- Instantiate the game engine with a default config
 engine <- startup

 -- Get the global random number generator
 g <- getStdGen

 run engine $ config { initialFn = mkInit g }
