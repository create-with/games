module Mario.MarioPlayground exposing (..)

-- IMPORTS

import Playground exposing (..)
import Playground.Extra exposing (..)



-- MAIN


main =
    game view update 0


view computer offset =
    [ rectangle lightBlue 256 240
    , square lightRed 16
        |> moveRight offset
    , viewBricks brickLocations
    ]


viewBricks : List ( Float, Float ) -> Shape
viewBricks =
    List.map (\( x, y ) -> viewBrick x y)
        >> group


viewBrick : Float -> Float -> Shape
viewBrick x y =
    tile 16 16 "tiles.png" 0
        |> move x y


update computer offset =
    offset + 0.05


brickLocations : List ( Float, Float )
brickLocations =
    [ ( -120, -96 )
    , ( -104, -96 )
    , ( -88, -96 )
    , ( -72, -96 )
    , ( -56, -96 )
    , ( -40, -96 )
    , ( -24, -96 )
    , ( -8, -96 )
    , ( 8, -96 )
    , ( 24, -96 )
    , ( 40, -96 )
    , ( 56, -96 )
    , ( 72, -96 )
    , ( 88, -96 )
    , ( 104, -96 )
    , ( 120, -96 )
    , ( -120, -112 )
    , ( -104, -112 )
    , ( -88, -112 )
    , ( -72, -112 )
    , ( -56, -112 )
    , ( -40, -112 )
    , ( -24, -112 )
    , ( -8, -112 )
    , ( 8, -112 )
    , ( 24, -112 )
    , ( 40, -112 )
    , ( 56, -112 )
    , ( 72, -112 )
    , ( 88, -112 )
    , ( 104, -112 )
    , ( 120, -112 )
    ]
