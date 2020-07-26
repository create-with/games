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

import Pong.Ball exposing (Ball)
import Pong.Window exposing (Window)
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes
import Util.Keyboard



-- MODEL


type Direction
    = Down
    | Up


type alias Paddle =
    { color : String
    , id : PaddleId
    , score : Int
    , x : Float
    , y : Float
    , vy : Float
    , width : Float
    , height : Float
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
    , x = 48.0
    , y = 200.0
    , vy = 600.0
    , width = 10.0
    , height = 60.0
    }


initialRightPaddle : Paddle
initialRightPaddle =
    { color = "lightpink"
    , id = Right
    , score = 0
    , x = 740.0
    , y = 300.0
    , vy = 475.0
    , width = 10.0
    , height = 60.0
    }



-- UPDATE


updateScore : Paddle -> Paddle
updateScore paddle =
    { paddle | score = paddle.score + 1 }


updateLeftPaddle : Maybe Direction -> Ball -> Float -> Paddle -> Paddle
updateLeftPaddle direction _ deltaTime paddle =
    case direction of
        Just Down ->
            { paddle | y = paddle.y + paddle.vy * deltaTime }

        Just Up ->
            { paddle | y = paddle.y - paddle.vy * deltaTime }

        Nothing ->
            paddle


updateRightPaddle : Ball -> Float -> Paddle -> Paddle
updateRightPaddle ball deltaTime paddle =
    if ball.y > paddle.y then
        { paddle | y = paddle.y + paddle.vy * deltaTime }

    else if ball.y < paddle.y then
        { paddle | y = paddle.y - paddle.vy * deltaTime }

    else
        paddle


updateYWithinWindow : Window -> Paddle -> Paddle
updateYWithinWindow window paddle =
    let
        topEdge =
            window.y

        bottomEdge =
            window.height - paddle.height
    in
    { paddle | y = clamp topEdge bottomEdge paddle.y }



-- COLLISIONS


ballHitPaddle : Ball -> Paddle -> Bool
ballHitPaddle ball paddle =
    ballHitLeftPaddle ball paddle || ballHitRightPaddle ball paddle


ballHitLeftPaddle : Ball -> Paddle -> Bool
ballHitLeftPaddle ball paddle =
    (paddle.y <= ball.y && ball.y <= paddle.y + paddle.height)
        && (paddle.x <= ball.x && ball.x <= paddle.x + paddle.width)
        && (ball.vx < 0)


ballHitRightPaddle : Ball -> Paddle -> Bool
ballHitRightPaddle ball paddle =
    (paddle.y <= ball.y && ball.y <= paddle.y + paddle.height)
        && (paddle.x <= ball.x + ball.width && ball.x <= paddle.x + paddle.width)
        && (ball.vx > 0)


getPaddleHitByBall : Ball -> Paddle -> Paddle -> Maybe Paddle
getPaddleHitByBall ball leftPaddle rightPaddle =
    if ballHitLeftPaddle ball leftPaddle then
        Just leftPaddle

    else if ballHitRightPaddle ball rightPaddle then
        Just rightPaddle

    else
        Nothing


getPaddleHitByBallDistanceFromCenter : Ball -> Paddle -> Float
getPaddleHitByBallDistanceFromCenter ball paddle =
    if ballHitPaddle ball paddle then
        let
            paddleCenter =
                paddle.height / 2
        in
        -- -100 for Paddle Top
        --    0 for Paddle Center
        --  100 for Paddle Bottom
        (ball.y - paddle.y - paddleCenter) * 100 / paddleCenter

    else
        0



-- HELPERS


playerKeyPressToDirection : Set String -> Maybe Direction
playerKeyPressToDirection playerKeyPress =
    if Util.Keyboard.playerPressedArrowUpKey playerKeyPress then
        Just Up

    else if Util.Keyboard.playerPressedArrowDownKey playerKeyPress then
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
