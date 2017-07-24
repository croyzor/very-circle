module Types where

import Linear.V2
import System.Random

type Angle = Double
type Velocity = Double
type Radius = Double
-- An obstacle has an angle (on the circle it's attached to) and a velocity
type Obstacle = (Angle, Velocity)
-- A circle has a radius and maybe an obstacle
type Circle = (Radius, Maybe Obstacle)

data Model = Model
  -- The width and height of the Helm window
  { window  :: V2 Int
  -- The radius of each circle on the screen, which may contain an obstacle
  , circles :: [Circle]
  -- (Angle on outer circle, velocity)
  , player :: (Angle, Velocity)
  -- Random number generator
  , gen :: StdGen
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

