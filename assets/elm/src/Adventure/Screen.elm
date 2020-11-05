module Adventure.Screen exposing (..)

-- IMPORTS

import Svg exposing (Svg)
import Svg.Attributes



-- MODEL


type alias Block =
    { color : String
    , x : Float
    , y : Float
    , width : Float
    , height : Float
    }


type alias BlockLocation =
    ( Int, Int )


type alias Screen =
    List BlockLocation


initialScreen : Screen
initialScreen =
    let
        rows =
            59
    in
    rows
        |> List.range 0
        |> List.map (\y -> ( 0, y ))



-- VIEW


createBlock : BlockLocation -> Block
createBlock ( x, y ) =
    Block "yellow" (toFloat x * 10.0) (toFloat y * 10.0) 10.0 10.0


viewScreen : Screen -> Svg a
viewScreen screen =
    initialScreen
        |> List.map createBlock
        |> List.map viewBlock
        |> Svg.g []


viewBlock : Block -> Svg a
viewBlock block =
    Svg.rect
        [ Svg.Attributes.fill <| block.color
        , Svg.Attributes.x <| String.fromFloat block.x
        , Svg.Attributes.y <| String.fromFloat block.y
        , Svg.Attributes.width <| String.fromFloat block.width
        , Svg.Attributes.height <| String.fromFloat block.height
        ]
        []
