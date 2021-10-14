module Mario.MarioPlayground exposing (..)

-- IMPORTS

import Playground exposing (..)
import Playground.Extra exposing (..)



-- MAIN


main : Program () (Playground Model) Msg
main =
    game view update init



-- MODEL


type alias Model =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , direction : Direction
    }


type Direction
    = Right
    | Left


init : Model
init =
    { x = -80
    , y = -80
    , vx = 0
    , vy = 0
    , direction = Right
    }



-- VIEW


view : Computer -> Model -> List Shape
view _ model =
    [ rectangle skyColor 256 240
    , viewMario model
    , viewBricks brickLocations
    ]


viewMario : Model -> Shape
viewMario ({ x, y } as model) =
    marioSprite model
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

        fallVelocity : Float
        fallVelocity =
            0.2

        jumpVelocity : Float
        jumpVelocity =
            0.15

        runVelocity : Float
        runVelocity =
            0.15

        walkVelocity : Float
        walkVelocity =
            0.11
    in
    if jumpKeyPressed computer.keyboard then
        model
            |> updateYVelocity jumpVelocity
            |> updateMario computer

    else if runRightKeysPressed computer.keyboard then
        model
            |> updateDirection Right
            |> updateXVelocity runVelocity
            |> updateMario computer

    else if runLeftKeysPressed computer.keyboard then
        model
            |> updateDirection Left
            |> updateXVelocity -runVelocity
            |> updateMario computer

    else if walkRightKeyPressed computer.keyboard then
        model
            |> updateDirection Right
            |> updateXVelocity walkVelocity
            |> updateMario computer

    else if walkLeftKeyPressed computer.keyboard then
        model
            |> updateDirection Left
            |> updateXVelocity -walkVelocity
            |> updateMario computer

    else if model.vy > 0 then
        model
            |> updateYVelocity -fallVelocity
            |> updateMario computer

    else
        model
            |> updateXVelocity 0
            |> updateYVelocity 0



-- UPDATES


updateDirection : Direction -> Model -> Model
updateDirection direction model =
    { model | direction = direction }


updateMario : Computer -> Model -> Model
updateMario computer model =
    { model
        | x = model.x + model.vx * toFloat computer.time.delta
        , y = model.y + model.vy * toFloat computer.time.delta
    }


updateXVelocity : Float -> Model -> Model
updateXVelocity velocity model =
    { model | vx = velocity }


updateYVelocity : Float -> Model -> Model
updateYVelocity velocity model =
    { model | vy = velocity }



-- PREDICATES


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
    keyboard.space || keyboard.up



-- jumpRightKeysPressed : Keyboard -> Bool
-- jumpRightKeysPressed keyboard =
--     keyboard.space && keyboard.right
-- jumpLeftKeysPressed : Keyboard -> Bool
-- jumpLeftKeysPressed keyboard =
--     keyboard.space && keyboard.left
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


type alias SpriteLocation =
    { xmin : Float
    , xmax : Float
    , ymin : Float
    , ymax : Float
    }


marioSprite : Model -> Shape
marioSprite model =
    case ( model.direction, model.y ) of
        ( Right, y ) ->
            if y > init.y then
                sprite "sprites.gif" marioJumpRight

            else
                sprite "sprites.gif" marioRight

        ( Left, y ) ->
            if y > init.y then
                sprite "sprites.gif" marioJumpLeft

            else
                sprite "sprites.gif" marioLeft


marioRight : SpriteLocation
marioRight =
    { xmin = 274
    , xmax = 290
    , ymin = 44
    , ymax = 60
    }


marioJumpRight : SpriteLocation
marioJumpRight =
    { xmin = 354
    , xmax = 370
    , ymin = 44
    , ymax = 60
    }


marioLeft : SpriteLocation
marioLeft =
    { xmin = 222
    , xmax = 238
    , ymin = 44
    , ymax = 60
    }


marioJumpLeft : SpriteLocation
marioJumpLeft =
    { xmin = 142
    , xmax = 158
    , ymin = 44
    , ymax = 60
    }



-- COLORS


skyColor : Color
skyColor =
    rgb 92 148 252
