module Pong.Window exposing
    ( Window
    , WindowEdge(..)
    , getWindowEdgeHitByBall
    , globalWindow
    , viewGameWindow
    , viewNet
    )

-- IMPORTS

import Pong.Ball
import Svg
import Svg.Attributes



-- MODEL


type alias Window =
    { backgroundColor : String
    , x : Int
    , y : Int
    , width : Int
    , height : Int
    }


type WindowEdge
    = Bottom
    | Left
    | Right
    | Top



-- GLOBAL


globalWindow : Window
globalWindow =
    { backgroundColor = "black"
    , x = 0
    , y = 0
    , width = 800
    , height = 600
    }



-- COLLISIONS


getWindowEdgeHitByBall : Pong.Ball.Ball -> Window -> Maybe WindowEdge
getWindowEdgeHitByBall ball window =
    if (ball.y + ball.height) >= window.height then
        Just Bottom

    else if (ball.x - ball.width) <= window.x then
        Just Left

    else if (ball.x + ball.width) >= window.width then
        Just Right

    else if (ball.y - ball.height) <= window.x then
        Just Top

    else
        Nothing



-- VIEW


viewGameWindow : Window -> Svg.Svg msg
viewGameWindow window =
    Svg.rect
        [ Svg.Attributes.fill window.backgroundColor
        , Svg.Attributes.x <| String.fromInt window.x
        , Svg.Attributes.y <| String.fromInt window.y
        , Svg.Attributes.width <| String.fromInt window.width
        , Svg.Attributes.height <| String.fromInt window.height
        ]
        []


viewNet : Window -> Svg.Svg msg
viewNet window =
    Svg.line
        [ Svg.Attributes.stroke "white"
        , Svg.Attributes.strokeDasharray "14, 14"
        , Svg.Attributes.strokeWidth "4"
        , Svg.Attributes.x1 <| String.fromInt <| (window.width // 2)
        , Svg.Attributes.x2 <| String.fromInt <| (window.width // 2)
        , Svg.Attributes.y1 <| String.fromInt window.y
        , Svg.Attributes.y2 <| String.fromInt window.height
        ]
        []
