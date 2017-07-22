module Drawing where

import Colours
import Helm.Engine.SDL (SDLEngine)
import Helm.Graphics2D

-- A thicker (blue) LineStyle for the static player circle
thickBlueLine :: LineStyle
thickBlueLine = defaultLine { lineColor = blue
                            , lineWidth = 4
                            }


