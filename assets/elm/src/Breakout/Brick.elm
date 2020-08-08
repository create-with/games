module Breakout.Brick exposing
    ( Brick
    , Bricks
    , State(..)
    , ballHitBrick
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
    , hitCount : Int
    , hitThreshold : Int
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
    , hitCount = 0
    , hitThreshold = 1
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
        |> List.foldr (insertBrick rowNumber) Dict.empty
        |> setRowColors color
        |> setRowPosition


insertBrick : Int -> Int -> (Bricks -> Bricks)
insertBrick rowNumber columnNumber =
    Dict.insert ( rowNumber, columnNumber ) defaultBrick


setRowColors : String -> Bricks -> Bricks
setRowColors color row =
    Dict.map (\_ brick -> { brick | color = color }) row


offsetFromTopOfScreen : Float
offsetFromTopOfScreen =
    80


setRowPosition : Bricks -> Bricks
setRowPosition row =
    Dict.map setBrickPosition row


setBrickPosition : ( Int, Int ) -> Brick -> Brick
setBrickPosition ( rowNumber, columnNumber ) brick =
    { brick
        | position =
            ( toFloat (columnNumber - 1) * brick.width
            , offsetFromTopOfScreen + toFloat rowNumber * brick.height
            )
    }



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
        |> Dict.filter (\( _, _ ) brick -> brick.hitCount < brick.hitThreshold)
        |> Dict.map viewBrick
        |> Dict.values
        |> Svg.g []


viewBrick : ( Int, Int ) -> Brick -> Svg a
viewBrick ( _, _ ) brick =
    Svg.rect
        [ Svg.Attributes.class "bounce-in-down"
        , Svg.Attributes.fill <| brick.color
        , Svg.Attributes.fillOpacity "1"
        , Svg.Attributes.x <| String.fromFloat <| Util.Vector.getX brick.position
        , Svg.Attributes.y <| String.fromFloat <| Util.Vector.getY brick.position
        , Svg.Attributes.width <| String.fromFloat brick.width
        , Svg.Attributes.height <| String.fromFloat brick.height
        , Svg.Attributes.stroke "black"
        , Svg.Attributes.strokeWidth "1"
        , Svg.Attributes.strokeOpacity "1"
        ]
        []
