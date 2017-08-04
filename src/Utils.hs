module Utils where

import Types

{-
Take a Circle and the cartesian coordinates describing the player's
position, and return whether or not a collision is occurring
-}
detectCollisions :: Circle -> (Angle, Radius) -> Bool
detectCollisions (_, Nothing) _ = False
detectCollisions (r1, Just (a1,_)) (a2,r2) = (abs (r2 - r1)) < 10
                                             && x' < 10
                                             && y' < 10
  where
  x' = (r1 * sin a1) - (r2 * sin a2)
  y' = (r1 * cos a1) - (r2 * cos a2)


