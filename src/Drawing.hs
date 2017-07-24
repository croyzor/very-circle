module Drawing where

import Colours
import Linear.V2
import Helm (Engine)
import Helm.Engine.SDL (SDLEngine)
import Helm.Graphics2D

-- A thicker (blue) LineStyle for the static player circle
thickBlueLine :: LineStyle
thickBlueLine = defaultLine { lineColor = blue
                            , lineWidth = 4
                            }

-- Helper to move a form according to a cartesian coordinate pair
-- (radius, angle)
moveCartesian :: Engine e => (Double,Double) -> Form e -> Form e
moveCartesian (r,a) form = move (pure r * V2 (cos a) (sin a)) form

redCircle :: Engine e => Form e
redCircle = filled red $ circle 10
