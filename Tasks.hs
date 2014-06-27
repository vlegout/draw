
module Tasks
( Arrow (..)
, arrows
, Diagram(..)
, diagrams
, Exec(..)
, execs
, height_sched
, margin_bottom
, margin_top
, margin_x_left
, n_tasks
, print_x_scale
, Text(..)
, texts
, tot_length
, unit_width
, VLine(..)
, vlines
) where

import Graphics.Rendering.Pango.Markup
import Graphics.Rendering.Pango.Enums

data Arrow = Arrow { x_arrow :: Double
                   , x_diagram :: Double
		   }

data Diagram = Diagram { title :: Markup
                       , x_axis :: String
                       , x_grid :: Double
                       , y_axis :: String
                       }

data Exec = Exec { start :: Double
                 , end :: Double
                 , color :: Int
                 , desc :: Markup
                 , diagram :: Double
                 , kind :: Int
                 , size :: Size
                 , execHeight :: Double
                 , alpha :: Double
                 }

data VLine = VLine { linex :: Double }

data Text = Text { textAbsc :: Double
                 , textStr :: String
                 , textSize :: Size
                 }

height_sched = 30.0
tot_length = 12.0
unit_width = 20.0

margin_bottom = 10.0
margin_top = 0.0
margin_x_left = 20.0

print_x_scale = True

n_tasks = 2

arrows = [ ]

diagrams = [ Diagram "" "t" 1 "π<sub>1</sub>"
           , Diagram "" "t" 1 "π<sub>2</sub>"
           ]

execs = [ Exec 0 2 0 "τ<sub>1</sub>" 1 0 SizeTiny 1.0 1.0
        , Exec 4 6 0 "τ<sub>1</sub>" 1 0 SizeTiny 1.0 1.0
        , Exec 8 10 0 "τ<sub>1</sub>" 1 0 SizeTiny 1.0 1.0
        , Exec 0 3.5 1 "τ<sub>2</sub>" 2 0 SizeTiny 1.0 1.0
        , Exec 6 9.5 1 "τ<sub>2</sub>" 2 0 SizeTiny 1.0 1.0
        ]

vlines = [ VLine 12 ]

texts = [ ]
