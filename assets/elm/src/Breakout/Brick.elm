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
    , height : Float
    , position : Vector
    , state : State
    , width : Float
    }


type alias Bricks =
    List Brick



-- INIT


initialBrick : Brick
initialBrick =
    { color = "white"
    , height = 16
    , position = ( 0, 0 )
    , state = On
    , width = 80
    }


initialBricks : Bricks
initialBricks =
    let
        topOfFirstRow =
            100
    in
    [ row "#F56565" (topOfFirstRow + (initialBrick.height * 0))
    , row "#ED8936" (topOfFirstRow + (initialBrick.height * 1))
    , row "#ECC94B" (topOfFirstRow + (initialBrick.height * 2))
    , row "#48BB78" (topOfFirstRow + (initialBrick.height * 3))
    , row "#4299E1" (topOfFirstRow + (initialBrick.height * 4))
    , row "#667EEA" (topOfFirstRow + (initialBrick.height * 5))
    ]
        |> Util.List.flatten


row : String -> Float -> Bricks
row color verticalPosition =
    [ Brick color initialBrick.height ( initialBrick.width * 0, verticalPosition ) On initialBrick.width
    , Brick color initialBrick.height ( initialBrick.width * 1, verticalPosition ) On initialBrick.width
    , Brick color initialBrick.height ( initialBrick.width * 2, verticalPosition ) On initialBrick.width
    , Brick color initialBrick.height ( initialBrick.width * 3, verticalPosition ) On initialBrick.width
    , Brick color initialBrick.height ( initialBrick.width * 4, verticalPosition ) On initialBrick.width
    , Brick color initialBrick.height ( initialBrick.width * 5, verticalPosition ) On initialBrick.width
    , Brick color initialBrick.height ( initialBrick.width * 6, verticalPosition ) On initialBrick.width
    , Brick color initialBrick.height ( initialBrick.width * 7, verticalPosition ) On initialBrick.width
    , Brick color initialBrick.height ( initialBrick.width * 8, verticalPosition ) On initialBrick.width
    , Brick color initialBrick.height ( initialBrick.width * 9, verticalPosition ) On initialBrick.width
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
        , Svg.Attributes.fillOpacity "0.9"
        , Svg.Attributes.x <| String.fromFloat x
        , Svg.Attributes.y <| String.fromFloat y
        , Svg.Attributes.width <| "80"
        , Svg.Attributes.height <| "16"
        , Svg.Attributes.stroke "white"
        , Svg.Attributes.strokeWidth "1"
        , Svg.Attributes.strokeOpacity "0.5"
        ]
        []
