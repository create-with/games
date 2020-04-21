module Breakout.Window exposing
    ( Window
    ,  WindowEdge(..)
       -- , getWindowEdgeHitByBall

    , globalWindow
    , viewGameWindow
    )

-- IMPORTS

import Svg exposing (Svg)
import Svg.Attributes



-- MODEL


type alias Window =
    { backgroundColor : String
    , x : Float
    , y : Float
    , width : Float
    , height : Float
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
    , x = 0.0
    , y = 0.0
    , width = 800.0
    , height = 600.0
    }



-- COLLISIONS
-- getWindowEdgeHitByBall : Ball -> Window -> Maybe WindowEdge
-- getWindowEdgeHitByBall ball window =
--     if (ball.y + ball.height) >= window.height then
--         Just Bottom
--     else if (ball.x - ball.width) <= window.x then
--         Just Left
--     else if (ball.x + ball.width) >= window.width then
--         Just Right
--     else if (ball.y - ball.height) <= window.x then
--         Just Top
--     else
--         Nothing
-- VIEW


viewGameWindow : Window -> Svg msg
viewGameWindow window =
    Svg.rect
        [ Svg.Attributes.fill <| window.backgroundColor
        , Svg.Attributes.x <| String.fromFloat window.x
        , Svg.Attributes.y <| String.fromFloat window.y
        , Svg.Attributes.width <| String.fromFloat window.width
        , Svg.Attributes.height <| String.fromFloat window.height
        ]
        []