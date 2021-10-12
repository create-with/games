module Mario.MarioPlayground exposing (..)

-- IMPORTS

import Playground exposing (..)
import Playground.Extra exposing (..)



-- MODEL


type alias Model =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    }


init : Model
init =
    { x = -80
    , y = -80
    , vx = 0
    , vy = 0
    }



-- MAIN


main : Program () (Playground Model) Msg
main =
    game view update init



-- VIEW


view : Computer -> Model -> List Shape
view _ model =
    [ rectangle skyColor 256 240
    , viewMario model
    , viewBricks brickLocations
    ]


viewMario : Model -> Shape
viewMario { x, y } =
    marioRightSprite
        |> move x y


viewBricks : List ( Float, Float ) -> Shape
viewBricks =
    List.map (\( x, y ) -> viewBrick x y)
        >> group


viewBrick : Float -> Float -> Shape
viewBrick x y =
    tile 16 16 "tiles.png" 0
        |> move x y



-- UPDATE


update : Computer -> Model -> Model
update computer model =
    -- NOTE: Pressing multiple keys must come before single key presses.
    if jumpRightKeysPressed computer.keyboard then
        { model
            | x = model.x + model.vx * toFloat computer.time.delta
            , y = model.y + model.vy * toFloat computer.time.delta
            , vx = 0.11
            , vy = 0.2
        }

    else if jumpLeftKeysPressed computer.keyboard then
        { model
            | x = model.x - model.vx * toFloat computer.time.delta
            , y = model.y + model.vy * toFloat computer.time.delta
            , vx = 0.11
            , vy = 0.2
        }

    else if runRightKeysPressed computer.keyboard then
        { model
            | x = model.x + model.vx * toFloat computer.time.delta
            , vx = 0.15
        }

    else if runLeftKeysPressed computer.keyboard then
        { model
            | x = model.x - model.vx * toFloat computer.time.delta
            , vx = 0.15
        }

    else if walkRightKeyPressed computer.keyboard then
        { model
            | x = model.x + model.vx * toFloat computer.time.delta
            , vx = 0.11
        }

    else if walkLeftKeyPressed computer.keyboard then
        { model
            | x = model.x - model.vx * toFloat computer.time.delta
            , vx = 0.11
        }

    else if jumpKeyPressed computer.keyboard then
        { model
            | y = model.y + model.vy * toFloat computer.time.delta
            , vy = 0.2
        }

    else if gravityShouldBeApplied model then
        { model
            | y = model.y - model.vy * toFloat computer.time.delta
            , vy = 0.2
        }

    else
        model


gravityShouldBeApplied : Model -> Bool
gravityShouldBeApplied { y, vy } =
    vy > 0 && y > init.y


walkRightKeyPressed : Keyboard -> Bool
walkRightKeyPressed keyboard =
    keyboard.right


runRightKeysPressed : Keyboard -> Bool
runRightKeysPressed keyboard =
    keyboard.right && keyboard.shift


walkLeftKeyPressed : Keyboard -> Bool
walkLeftKeyPressed keyboard =
    keyboard.left


runLeftKeysPressed : Keyboard -> Bool
runLeftKeysPressed keyboard =
    keyboard.left && keyboard.shift


jumpKeyPressed : Keyboard -> Bool
jumpKeyPressed keyboard =
    keyboard.up


jumpRightKeysPressed : Keyboard -> Bool
jumpRightKeysPressed keyboard =
    keyboard.up && keyboard.right


jumpLeftKeysPressed : Keyboard -> Bool
jumpLeftKeysPressed keyboard =
    keyboard.up && keyboard.left



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
