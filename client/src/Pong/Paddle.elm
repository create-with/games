module Pong.Paddle exposing
    ( Direction(..)
    , Paddle
    , PaddleId(..)
    , getPaddleHitByBall
    , getPaddleHitByBallDistanceFromCenter
    , initialLeftPaddle
    , initialRightPaddle
    , paddleIdToString
    , playerKeyPressToDirection
    , updateLeftPaddle
    , updateRightPaddle
    , updateScore
    , updateYWithinWindow
    , viewPaddle
    , viewPaddleScore
    )

-- IMPORTS

import Keyboard
import Pong.Ball
import Pong.Window
import Set
import Svg
import Svg.Attributes



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


ballHitPaddle : Pong.Ball.Ball -> Paddle -> Bool
ballHitPaddle ball paddle =
    ballHitLeftPaddle ball paddle || ballHitRightPaddle ball paddle


ballHitLeftPaddle : Pong.Ball.Ball -> Paddle -> Bool
ballHitLeftPaddle ball paddle =
    (paddle.y <= ball.y && ball.y <= paddle.y + paddle.height)
        && (paddle.x <= ball.x && ball.x <= paddle.x + paddle.width)
        -- && (ball.vx < 0)


ballHitRightPaddle : Pong.Ball.Ball -> Paddle -> Bool
ballHitRightPaddle ball paddle =
    (paddle.y <= ball.y && ball.y <= paddle.y + paddle.height)
        && (paddle.x <= ball.x + ball.width && ball.x <= paddle.x + paddle.width)
        -- && (ball.vx > 0)


getPaddleHitByBall : Pong.Ball.Ball -> Paddle -> Paddle -> Maybe Paddle
getPaddleHitByBall ball leftPaddle rightPaddle =
    if ballHitLeftPaddle ball leftPaddle then
        Just leftPaddle

    else if ballHitRightPaddle ball rightPaddle then
        Just rightPaddle

    else
        Nothing


getPaddleHitByBallDistanceFromCenter : Pong.Ball.Ball -> Maybe Paddle -> Maybe Int
getPaddleHitByBallDistanceFromCenter ball maybePaddle =
    case maybePaddle of
        Just paddle ->
            if ballHitPaddle ball paddle then
                let
                    paddleCenter =
                        paddle.height // 2
                in
                -- Top -100, Center 0, Bottom 100
                Just <| (ball.y - paddle.y - paddleCenter) * 100 // paddleCenter

            else
                Nothing

        Nothing ->
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



-- VIEW


viewPaddle : Paddle -> Svg.Svg msg
viewPaddle paddle =
    Svg.rect
        [ Svg.Attributes.fill paddle.color
        , Svg.Attributes.x <| String.fromInt paddle.x
        , Svg.Attributes.y <| String.fromInt paddle.y
        , Svg.Attributes.width <| String.fromInt paddle.width
        , Svg.Attributes.height <| String.fromInt paddle.height
        ]
        []


viewPaddleScore : Int -> Pong.Window.Window -> Int -> Svg.Svg msg
viewPaddleScore score window positionOffset =
    Svg.text_
        [ Svg.Attributes.fill "white"
        , Svg.Attributes.fontFamily "monospace"
        , Svg.Attributes.fontSize "80"
        , Svg.Attributes.fontWeight "bold"
        , Svg.Attributes.x <| String.fromInt <| (window.width // 2) + positionOffset
        , Svg.Attributes.y "100"
        ]
        [ Svg.text <| String.fromInt score ]
