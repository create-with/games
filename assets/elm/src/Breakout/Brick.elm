module Breakout.Brick exposing
    ( Brick
    , Bricks
    , ballHitBrick
    , filterDestroyedBricks
    , getBrickHitByBall
    , incrementBrickHitCount
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


type alias Brick =
    { color : String
    , height : Float
    , hitCount : Int
    , hitThreshold : Int
    , position : Vector
    , strokeColor : String
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
    , position = ( 0, 0 )
    , strokeColor = "black"
    , width = 80
    }


initialBricks : Bricks
initialBricks =
    buildRow 1 "#f56565" "white"
        |> Dict.union (buildRow 2 "#ed8936" "black")
        |> Dict.union (buildRow 3 "#ecc94b" "black")
        |> Dict.union (buildRow 4 "#48bb78" "black")
        |> Dict.union (buildRow 5 "#4299e1" "black")
        |> Dict.union (buildRow 6 "#667eea" "black")
        |> setHardRow 1


buildRow : Int -> String -> String -> Bricks
buildRow rowNumber color strokeColor =
    List.range 1 10
        |> List.foldr (insertBrick rowNumber) Dict.empty
        |> setRowColors color strokeColor
        |> setRowPosition


insertBrick : Int -> Int -> (Bricks -> Bricks)
insertBrick rowNumber columnNumber =
    Dict.insert ( rowNumber, columnNumber ) defaultBrick


setRowColors : String -> String -> Bricks -> Bricks
setRowColors color strokeColor row =
    Dict.map (\_ brick -> { brick | color = color, strokeColor = strokeColor }) row


setRowPosition : Bricks -> Bricks
setRowPosition row =
    Dict.map setBrickPosition row


setBrickPosition : ( Int, Int ) -> Brick -> Brick
setBrickPosition ( rowNumber, columnNumber ) brick =
    let
        rowHeight =
            toFloat rowNumber * brick.height

        paddingForStroke =
            case rowNumber of
                1 ->
                    0

                _ ->
                    2

        offsetHorizontalColumn =
            toFloat (columnNumber - 1)

        paddingFromTopOfWindow =
            toFloat 80
    in
    { brick
        | position =
            ( offsetHorizontalColumn * brick.width
            , paddingFromTopOfWindow + rowHeight + paddingForStroke
            )
    }


setHardRow : Int -> Bricks -> Bricks
setHardRow rowNumber bricks =
    Dict.map (setHardBrick rowNumber) bricks


setHardBrick : Int -> ( Int, Int ) -> Brick -> Brick
setHardBrick targetRowNumber ( rowNumber, _ ) brick =
    if targetRowNumber == rowNumber then
        { brick | hitThreshold = 2 }

    else
        brick



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


getBrickHitByBall : Ball -> Bricks -> Maybe Brick
getBrickHitByBall ball bricks =
    -- REFACTOR
    bricks
        |> Dict.filter (\_ brick -> ballHitBrick ball brick)
        |> Dict.values
        |> List.head


brickHitCountPassedThreshold : ( Int, Int ) -> Brick -> Bool
brickHitCountPassedThreshold _ brick =
    brick.hitCount < brick.hitThreshold


incrementBrickHitCount : Ball -> ( Int, Int ) -> Brick -> Brick
incrementBrickHitCount ball _ brick =
    if ballHitBrick ball brick then
        { brick | hitCount = brick.hitCount + 1 }

    else
        brick


filterDestroyedBricks : Bricks -> Bricks
filterDestroyedBricks bricks =
    Dict.filter brickHitCountPassedThreshold bricks



-- VIEW


viewBricks : Bricks -> Svg a
viewBricks bricks =
    bricks
        |> Dict.map viewBrick
        |> Dict.values
        |> Svg.g []


viewBrick : ( Int, Int ) -> Brick -> Svg a
viewBrick _ brick =
    Svg.rect
        [ Svg.Attributes.class "bounce-in-down"
        , Svg.Attributes.fill <| brick.color
        , Svg.Attributes.fillOpacity "1"
        , Svg.Attributes.x <| String.fromFloat <| Util.Vector.getX brick.position
        , Svg.Attributes.y <| String.fromFloat <| Util.Vector.getY brick.position
        , Svg.Attributes.width <| String.fromFloat brick.width
        , Svg.Attributes.height <| String.fromFloat brick.height
        , Svg.Attributes.stroke <| brick.strokeColor
        , Svg.Attributes.strokeWidth "2"
        , Svg.Attributes.strokeOpacity <| String.fromFloat <| brickStrokeOpacity brick
        ]
        []


brickStrokeOpacity : Brick -> Float
brickStrokeOpacity brick =
    toFloat (brick.hitThreshold - brick.hitCount) / toFloat brick.hitThreshold
