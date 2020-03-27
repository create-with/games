module Pong.Paddle exposing
    ( Direction(..)
    , Paddle
    , PaddleId(..)
    , getPaddleHitByBall
    , initialLeftPaddle
    , initialRightPaddle
    , paddleIdToString
    , playerKeyPressToDirection
    , updateLeftPaddle
    , updateRightPaddle
    , updateScore
    , updateYWithinWindow
    )

-- IMPORTS

import Keyboard
import Pong.Ball
import Pong.Window
import Set



-- MODEL


type Direction
    = Down
    | Up


type alias Paddle =
    { color : String
    , id : PaddleId
    , score : Int
    , x : Int
    , y : Int
    , vy : Float
    , width : Int
    , height : Int
    }


type PaddleId
    = Left
    | Right



-- INIT


initialLeftPaddle : Paddle
initialLeftPaddle =
    { color = "lightblue"
    , id = Left
    , score = 0
    , x = 48
    , y = 200
    , vy = 500.0
    , width = 10
    , height = 60
    }


initialRightPaddle : Paddle
initialRightPaddle =
    { color = "lightpink"
    , id = Right
    , score = 0
    , x = 740
    , y = 300
    , vy = 500.0
    , width = 10
    , height = 60
    }



-- UPDATE


updateScore : Paddle -> Paddle
updateScore paddle =
    { paddle | score = paddle.score + 1 }


updateLeftPaddle : Maybe Direction -> Pong.Ball.Ball -> Float -> Paddle -> Paddle
updateLeftPaddle direction _ deltaTime paddle =
    case direction of
        Just Down ->
            { paddle | y = round <| toFloat paddle.y + paddle.vy * deltaTime }

        Just Up ->
            { paddle | y = round <| toFloat paddle.y - paddle.vy * deltaTime }

        Nothing ->
            paddle


updateRightPaddle : Pong.Ball.Ball -> Float -> Paddle -> Paddle
updateRightPaddle ball deltaTime paddle =
    if ball.y > paddle.y then
        { paddle | y = round <| toFloat paddle.y + paddle.vy * deltaTime }

    else if ball.y < paddle.y then
        { paddle | y = round <| toFloat paddle.y - paddle.vy * deltaTime }

    else
        paddle


updateYWithinWindow : Pong.Window.Window -> Paddle -> Paddle
updateYWithinWindow window paddle =
    let
        topEdge =
            window.y

        bottomEdge =
            window.height - paddle.height
    in
    { paddle | y = Basics.clamp topEdge bottomEdge paddle.y }



-- COLLISIONS


ballHitLeftPaddle : Pong.Ball.Ball -> Paddle -> Bool
ballHitLeftPaddle ball paddle =
    (paddle.y <= ball.y && ball.y <= paddle.y + paddle.height)
        && (paddle.x <= ball.x && ball.x <= paddle.x + paddle.width)


ballHitRightPaddle : Pong.Ball.Ball -> Paddle -> Bool
ballHitRightPaddle ball paddle =
    (paddle.y <= ball.y && ball.y <= paddle.y + paddle.height)
        && (paddle.x <= ball.x + ball.width && ball.x <= paddle.x + paddle.width)


getPaddleHitByBall : Pong.Ball.Ball -> Paddle -> Paddle -> Maybe Paddle
getPaddleHitByBall ball leftPaddle rightPaddle =
    if ballHitLeftPaddle ball leftPaddle then
        Just leftPaddle

    else if ballHitRightPaddle ball rightPaddle then
        Just rightPaddle

    else
        Nothing



-- HELPERS


playerKeyPressToDirection : Set.Set String -> Maybe Direction
playerKeyPressToDirection playerKeyPress =
    if Keyboard.playerPressedArrowUpKey playerKeyPress then
        Just Up

    else if Keyboard.playerPressedArrowDownKey playerKeyPress then
        Just Down

    else
        Nothing


paddleIdToString : PaddleId -> String
paddleIdToString paddleId =
    case paddleId of
        Left ->
            "Left"

        Right ->
            "Right"
