module Mario.MarioPlayground exposing (..)

-- IMPORTS

import Playground exposing (..)
import Playground.Extra exposing (..)



-- MAIN


main : Program () (Playground Float) Msg
main =
    game view update 0



-- VIEW


view : Computer -> Float -> List Shape
view _ offset =
    [ rectangle skyColor 256 240
    , viewMario offset
    , viewBricks brickLocations
    ]


viewMario : Float -> Shape
viewMario offset =
    let
        ( startingX, startingY ) =
            ( -80, -80 )
    in
    marioRightSprite
        |> move startingX startingY
        |> moveRight offset


viewBricks : List ( Float, Float ) -> Shape
viewBricks =
    List.map (\( x, y ) -> viewBrick x y)
        >> group


viewBrick : Float -> Float -> Shape
viewBrick x y =
    tile 16 16 "tiles.png" 0
        |> move x y



-- UPDATE


update : Computer -> Float -> Float
update computer offset =
    -- NOTE: Pressing multiple keys must come before single key presses.
    if runRightKeysPressed computer.keyboard then
        offset + 2.5

    else if runLeftKeysPressed computer.keyboard then
        offset - 2.5

    else if rightKeyPressed computer.keyboard then
        offset + 1.5

    else if leftKeyPressed computer.keyboard then
        offset - 1.5

    else
        offset


rightKeyPressed : Keyboard -> Bool
rightKeyPressed keyboard =
    keyboard.right


runRightKeysPressed : Keyboard -> Bool
runRightKeysPressed keyboard =
    keyboard.right && keyboard.shift


leftKeyPressed : Keyboard -> Bool
leftKeyPressed keyboard =
    keyboard.left


runLeftKeysPressed : Keyboard -> Bool
runLeftKeysPressed keyboard =
    keyboard.left && keyboard.shift



-- TILES


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



-- SPRITES


marioRightSprite : Shape
marioRightSprite =
    sprite "sprites.gif"
        { xmin = 274
        , xmax = 290
        , ymin = 44
        , ymax = 60
        }



-- COLORS


skyColor : Color
skyColor =
    rgb 92 148 252
