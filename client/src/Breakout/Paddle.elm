module Breakout.Paddle exposing
    ( Direction(..)
    , Paddle
    , initialPaddle
    , keepPaddleWithinWindow
    , playerKeyPressToDirection
       -- , updatePaddle

    , updateScore
    , viewPaddle
    , viewPaddleScore
    )

-- IMPORTS


import Breakout.Window exposing (Window)
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes
import Util.Keyboard



-- MODEL


type Direction
    = Left
    | Right


type alias Paddle =
    { color : String
    , score : Int
    , x : Float
    , y : Float
    , vx : Float
    , width : Float
    , height : Float
    }



-- INIT


initialPaddle : Paddle
initialPaddle =
    { color = "lightblue"
    , score = 0
    , x = 380.0
    , y = 550.0
    , vx = 600.0
    , width = 60.0
    , height = 10.0
    }



-- UPDATE


updateScore : Paddle -> Paddle
updateScore paddle =
    { paddle | score = paddle.score + 1 }



-- updatePaddle : Paddle -> Paddle


keepPaddleWithinWindow : Window -> Paddle -> Paddle
keepPaddleWithinWindow window paddle =
    let
        leftEdge =
            0

        rightEdge =
            window.width - paddle.width
    in
    { paddle | x = clamp leftEdge rightEdge paddle.x }



-- COLLISIONS
-- ballHitPaddle : Ball -> Paddle -> Bool
-- ballHitPaddle ball paddle =
--     (paddle.y <= ball.y && ball.y <= paddle.y + paddle.height)
--         && (paddle.x <= ball.x && ball.x <= paddle.x + paddle.width)
--         && (ball.vx < 0)
-- HELPERS


playerKeyPressToDirection : Set String -> Maybe Direction
playerKeyPressToDirection playerKeyPress =
    if Util.Keyboard.playerPressedArrowLeftKey playerKeyPress then
        Just Left

    else if Util.Keyboard.playerPressedArrowRightKey playerKeyPress then
        Just Right

    else
        Nothing



-- VIEW


viewPaddle : Paddle -> Svg msg
viewPaddle paddle =
    Svg.rect
        [ Svg.Attributes.fill <| paddle.color
        , Svg.Attributes.x <| String.fromFloat paddle.x
        , Svg.Attributes.y <| String.fromFloat paddle.y
        , Svg.Attributes.width <| String.fromFloat paddle.width
        , Svg.Attributes.height <| String.fromFloat paddle.height
        ]
        []


viewPaddleScore : Int -> Window -> Float -> Svg msg
viewPaddleScore score window positionOffset =
    Svg.text_
        [ Svg.Attributes.fill "white"
        , Svg.Attributes.fontFamily "monospace"
        , Svg.Attributes.fontSize "80"
        , Svg.Attributes.fontWeight "bold"
        , Svg.Attributes.x <| String.fromFloat <| (window.width / 2) + positionOffset
        , Svg.Attributes.y "100"
        ]
        [ Svg.text <| String.fromInt score ]
