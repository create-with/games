module Pong.Window exposing
    ( Window
    , WindowEdge(..)
    , ballHitEdge
    , globalWindow
    )

-- IMPORTS

import Pong.Ball



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


ballHitEdge : Pong.Ball.Ball -> Window -> Maybe WindowEdge
ballHitEdge ball window =
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
