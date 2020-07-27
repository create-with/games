module Breakout.Brick exposing
    ( Brick
    , Bricks
    , State(..)
    , getBrickHitByBall
    , hideBrickHitByBall
    , initialBricks
    , viewBricks
    )

-- IMPORTS

import Breakout.Ball exposing (Ball)
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
    , Brick color initialBrick.height ( initialBrick.width * 4, verticalPosition ) Off initialBrick.width
    , Brick color initialBrick.height ( initialBrick.width * 5, verticalPosition ) On initialBrick.width
    , Brick color initialBrick.height ( initialBrick.width * 6, verticalPosition ) On initialBrick.width
    , Brick color initialBrick.height ( initialBrick.width * 7, verticalPosition ) On initialBrick.width
    , Brick color initialBrick.height ( initialBrick.width * 8, verticalPosition ) On initialBrick.width
    , Brick color initialBrick.height ( initialBrick.width * 9, verticalPosition ) On initialBrick.width
    ]



-- COLLISIONS


ballHitBrick : Ball -> Brick -> Bool
ballHitBrick ball brick =
    let
        ( ballX, ballY ) =
            ball.position

        ( brickX, brickY ) =
            brick.position
    in
    (brickX <= ballX && ballX <= brickX + brick.width)
        && (brickY <= ballY && ballY <= brickY + brick.height)


getBrickHitByBall : Ball -> Brick -> Maybe Brick
getBrickHitByBall ball brick =
    if ballHitBrick ball brick then
        Just brick

    else
        Nothing


hideBrickHitByBall : List Brick -> Brick -> Brick
hideBrickHitByBall bricksHitByBall brick =
    if List.member brick bricksHitByBall then
        { brick | state = Off }

    else
        brick



-- VIEW


viewBricks : List Brick -> Svg a
viewBricks bricks =
    bricks
        |> List.filter (\brick -> brick.state == On)
        |> List.map viewBrick
        |> Svg.g []


viewBrick : Brick -> Svg a
viewBrick brick =
    let
        ( x, y ) =
            brick.position
    in
    Svg.rect
        [ Svg.Attributes.fill <| brick.color
        , Svg.Attributes.fillOpacity "1"
        , Svg.Attributes.x <| String.fromFloat x
        , Svg.Attributes.y <| String.fromFloat y
        , Svg.Attributes.width <| "80"
        , Svg.Attributes.height <| "16"
        , Svg.Attributes.stroke "white"
        , Svg.Attributes.strokeWidth "1"
        , Svg.Attributes.strokeOpacity "0.5"
        ]
        []
