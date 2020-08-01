module Breakout.Paddle exposing
    ( Direction(..)
    , Paddle
    , ballHitPaddle
    , initialPaddle
    , keepPaddleWithinWindow
    , playerKeyPressToDirection
    , updatePaddle
    , updateScore
    , viewPaddle
    , viewPaddleScore
    )

-- IMPORTS

import Breakout.Ball exposing (Ball)
import Breakout.Window exposing (Window)
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes
import Util.Keyboard
import Util.Vector exposing (Vector)



-- MODEL


type Direction
    = Left
    | Right


type alias Paddle =
    { score : Int
    , position : Vector
    , vx : Float
    , width : Float
    , height : Float
    }



-- INIT


initialPaddle : Paddle
initialPaddle =
    { score = 0
    , position = ( 380.0, 550.0 )
    , vx = 600.0
    , width = 80.0
    , height = 20.0
    }



-- UPDATE


updateScore : Paddle -> Paddle
updateScore paddle =
    { paddle | score = paddle.score + 1 }



-- updatePaddle : Paddle -> Paddle


updatePaddle : Maybe Direction -> Float -> Paddle -> Paddle
updatePaddle maybeDirection deltaTime paddle =
    let
        ( x, y ) =
            paddle.position
    in
    case maybeDirection of
        Just Left ->
            { paddle | position = ( x - paddle.vx * deltaTime, y ) }

        Just Right ->
            { paddle | position = ( x + paddle.vx * deltaTime, y ) }

        Nothing ->
            paddle


keepPaddleWithinWindow : Window -> Paddle -> Paddle
keepPaddleWithinWindow window paddle =
    let
        ( x, y ) =
            paddle.position

        leftEdge =
            0

        rightEdge =
            window.width - paddle.width
    in
    { paddle | position = ( clamp leftEdge rightEdge x, y ) }



-- COLLISIONS


ballHitPaddle : Ball -> Paddle -> Bool
ballHitPaddle ball paddle =
    let
        ( ballX, ballY ) =
            ball.position

        ( _, ballVy ) =
            ball.velocity

        ( paddleX, paddleY ) =
            paddle.position
    in
    (paddleY <= ballY + ball.height)
        && (paddleX <= ballX && ballX <= paddleX + paddle.width)
        && (ballVy > 0)



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
    Svg.image
        [ Svg.Attributes.xlinkHref "/images/pixel-paddle.png"
        , Svg.Attributes.x <| String.fromFloat <| Util.Vector.getX paddle.position
        , Svg.Attributes.y <| String.fromFloat <| Util.Vector.getY paddle.position
        , Svg.Attributes.width <| String.fromFloat paddle.width
        , Svg.Attributes.height <| String.fromFloat paddle.height
        ]
        []


viewPaddleScore : Int -> Svg msg
viewPaddleScore score =
    Svg.text_
        [ Svg.Attributes.class "font-retro"
        , Svg.Attributes.fill "white"
        , Svg.Attributes.fontSize "12"
        , Svg.Attributes.fontWeight "bold"
        , Svg.Attributes.textAnchor "middle"
        , Svg.Attributes.x "50%"
        , Svg.Attributes.y "22"
        ]
        [ Svg.text <| String.toUpper <| "Points " ++ String.fromInt score ]
