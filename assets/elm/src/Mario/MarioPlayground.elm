module Mario.MarioPlayground exposing (..)

-- IMPORTS

import Playground exposing (..)
import Playground.Extra exposing (..)



-- MAIN


main =
    game view update 0


view computer offset =
    [ rectangle lightBlue 256.0 240.0
    , square lightRed 16.0
        |> moveRight offset
    , tile 16 16 "tiles.png" 0
    ]


update computer offset =
    offset + 0.05
