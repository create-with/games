module Breakout.Brick exposing
    ( Brick
    , Bricks
    , initialBricks
    , viewBricks
    )

-- IMPORTS

import Breakout.Vector exposing (Vector)
import Svg exposing (Svg)
import Svg.Attributes
import Util.List



-- MODEL


type State
    = On
    | Off


type alias Brick =
    { color : String
    , position : Vector
    , state : State
    }


type alias Bricks =
    List Brick



-- INIT


initialBricks : List Brick
initialBricks =
    [ row "#F56565" 100
    , row "#ED8936" 112
    , row "#F6AD55" 124
    , row "#ECC94B" 136
    , row "#48BB78" 148
    , row "#4299E1" 160
    ] |> Util.List.flatten


row : String -> Float -> List Brick
row color verticalPosition =
    [ Brick color ( 0, verticalPosition ) On
    , Brick color ( 80, verticalPosition ) On
    , Brick color ( 160, verticalPosition ) On
    , Brick color ( 240, verticalPosition ) On
    , Brick color ( 320, verticalPosition ) On
    , Brick color ( 400, verticalPosition ) On
    , Brick color ( 480, verticalPosition ) On
    , Brick color ( 560, verticalPosition ) On
    , Brick color ( 640, verticalPosition ) On
    , Brick color ( 720, verticalPosition ) On
    ]


-- VIEW


viewBricks : List Brick -> Svg a
viewBricks bricks =
    Svg.g []
        (List.map viewBrick bricks)


viewBrick : Brick -> Svg a
viewBrick brick =
    let
        ( x, y ) =
            brick.position
    in
    Svg.rect
        [ Svg.Attributes.fill <| brick.color
        , Svg.Attributes.x <| String.fromFloat x
        , Svg.Attributes.y <| String.fromFloat y
        , Svg.Attributes.width <| "80"
        , Svg.Attributes.height <| "12"
        ]
        []

