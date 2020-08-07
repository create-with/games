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

import Array exposing (Array)
import Breakout.Ball exposing (Ball)
import Dict exposing (Dict)
import Svg exposing (Svg)
import Svg.Attributes
import Util.List
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
    Dict RowNumber (List Brick)


type alias RowNumber =
    Int


type alias RowData =
    { color : String
    , yPosition : Float
    }



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
    Dict.empty
        |> Dict.insert 1 (createRow 0 10 { color = "#F56565", yPosition = defaultBrick.height * 1 + 100 } setupBricks)
        |> Dict.insert 2 (createRow 10 20 { color = "#ED8936", yPosition = defaultBrick.height * 2 + 100 } setupBricks)
        |> Dict.insert 3 (createRow 20 30 { color = "#ECC94B", yPosition = defaultBrick.height * 3 + 100 } setupBricks)
        |> Dict.insert 4 (createRow 30 40 { color = "#48BB78", yPosition = defaultBrick.height * 4 + 100 } setupBricks)
        |> Dict.insert 5 (createRow 40 50 { color = "#4299E1", yPosition = defaultBrick.height * 5 + 100 } setupBricks)
        |> Dict.insert 6 (createRow 50 60 { color = "#667EEA", yPosition = defaultBrick.height * 6 + 100 } setupBricks)



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
        |> Dict.values
        |> Util.List.flatten
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
        , Svg.Attributes.width <| String.fromFloat brick.width
        , Svg.Attributes.height <| String.fromFloat brick.height
        , Svg.Attributes.stroke "white"
        , Svg.Attributes.strokeWidth "2"
        , Svg.Attributes.strokeOpacity "0.5"
        ]
        []



-- REFACTOR


setupBricks : Array ( Int, Brick )
setupBricks =
    Array.initialize 60 setBrickIds


setBrickIds : Int -> ( Int, Brick )
setBrickIds index =
    ( index + 1, { defaultBrick | id = index + 1 } )


createRow : Int -> Int -> RowData -> Array ( Int, Brick ) -> List Brick
createRow start end rowData bricks =
    bricks
        |> Array.slice start end
        |> Array.toList
        |> List.map (setRowData rowData)


setRowData : RowData -> ( Int, Brick ) -> Brick
setRowData { color, yPosition } ( index, brick ) =
    { brick
        | color = color
        , position =
            ( defaultBrick.width * toFloat (remainderBy 10 (index - 1))
            , yPosition
            )
    }
