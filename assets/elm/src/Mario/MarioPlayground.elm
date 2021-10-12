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
    let
        _ =
            Debug.log "model" model
    in
    -- NOTE: Pressing multiple keys must come before single key presses.
    if runRightKeysPressed computer.keyboard then
        { model
            | x = model.x + model.vx * toFloat computer.time.delta
            , vx = 0.15
        }

    else if runLeftKeysPressed computer.keyboard then
        { model
            | x = model.x - model.vx * toFloat computer.time.delta
            , vx = 0.15
        }

    else if rightKeyPressed computer.keyboard then
        { model
            | x = model.x + model.vx * toFloat computer.time.delta
            , vx = 0.12
        }

    else if leftKeyPressed computer.keyboard then
        { model
            | x = model.x - model.vx * toFloat computer.time.delta
            , vx = 0.12
        }

    else if upKeyPressed computer.keyboard then
        -- JUMP
        { model
            | y = model.y + model.vy * toFloat computer.time.delta
            , vy = 0.15
        }

    else if model.vy > 0 && model.y > init.y then
        -- GRAVITY
        { model
            | y = model.y - model.vy * toFloat computer.time.delta
            , vy = 0.15
        }

    else
        model


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


upKeyPressed : Keyboard -> Bool
upKeyPressed keyboard =
    keyboard.up



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
