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
import Dict exposing (Dict)
import Svg exposing (Svg)
import Svg.Attributes
import Util.Vector exposing (Vector)



-- MODEL


type State
    = On
    | Off


type alias Brick =
    { color : String
    , height : Float
    , id : Int
    , position : Vector
    , state : State
    , width : Float
    }


type alias Bricks =
    Dict ( Int, Int ) Brick



-- INIT


defaultBrick : Brick
defaultBrick =
    { color = "white"
    , height = 16
    , id = 0
    , position = ( 0, 0 )
    , state = On
    , width = 80
    }


initialBricks : Bricks
initialBricks =
    buildRow 1 "#f56565"
        |> Dict.union (buildRow 2 "#ed8936")
        |> Dict.union (buildRow 3 "#ecc94b")
        |> Dict.union (buildRow 4 "#48bb78")
        |> Dict.union (buildRow 5 "#4299e1")
        |> Dict.union (buildRow 6 "#667eea")


buildRow : Int -> String -> Bricks
buildRow rowNumber color =
    List.range 1 10
        |> List.foldr (\columnNumber -> Dict.insert ( rowNumber, columnNumber ) defaultBrick) Dict.empty
        |> colorizeRow color


colorizeRow : String -> Bricks -> Dict ( Int, Int ) Brick
colorizeRow color row =
    Dict.map (\_ brick -> { brick | color = color }) row



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


viewBricks : Bricks -> Svg a
viewBricks bricks =
    bricks
        |> Dict.map viewBrick
        |> Dict.values
        |> Svg.g []


viewBrick : ( Int, Int ) -> Brick -> Svg a
viewBrick ( row, column ) brick =
    let
        offsetFromTopOfScreen =
            80
    in
    Svg.rect
        [ Svg.Attributes.fill <| brick.color
        , Svg.Attributes.fillOpacity "1"
        , Svg.Attributes.x <| String.fromFloat (toFloat (column - 1) * brick.width)
        , Svg.Attributes.y <| String.fromFloat (offsetFromTopOfScreen + toFloat row * brick.height)
        , Svg.Attributes.width <| String.fromFloat brick.width
        , Svg.Attributes.height <| String.fromFloat brick.height
        , Svg.Attributes.stroke "black"
        , Svg.Attributes.strokeWidth "1"
        , Svg.Attributes.strokeOpacity "1"
        ]
        []
