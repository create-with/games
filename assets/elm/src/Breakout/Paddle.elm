module Breakout.Paddle exposing
    ( Direction(..)
    , Paddle
    , ballHitPaddle
    , getPaddleHitByBallDistanceFromCenter
    , initialPaddle
    , keepPaddleWithinWindow
    , playerKeyPressToDirection
    , updateLives
    , updatePaddle
    , updateScore
    , viewLives
    , viewPaddle
    , viewPaddleScore
    )

-- IMPORTS

import Breakout.Ball exposing (Ball)
import Breakout.Brick exposing (Brick)
import Breakout.Window exposing (Window, WindowEdge)
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
    { height : Float
    , lives : Int
    , position : Vector
    , score : Int
    , vx : Float
    , width : Float
    }



-- INIT


initialPaddle : Paddle
initialPaddle =
    { height = 20.0
    , lives = 3
    , position = ( 380.0, 550.0 )
    , score = 0
    , vx = 700.0
    , width = 80.0
    }



-- UPDATE


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


updateLives : Maybe WindowEdge -> Paddle -> Paddle
updateLives maybeWindowEdge paddle =
    case maybeWindowEdge of
        Just Breakout.Window.Bottom ->
            { paddle | lives = clamp 0 paddle.lives <| paddle.lives - 1 }

        Just _ ->
            paddle

        Nothing ->
            paddle


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


updateScore : Maybe Brick -> Paddle -> Paddle
updateScore maybeBrick paddle =
    case maybeBrick of
        Just _ ->
            { paddle | score = paddle.score + 100 }

        Nothing ->
            paddle



-- COLLISIONS


ballHitPaddle : Ball -> Paddle -> Maybe Paddle
ballHitPaddle ball paddle =
    let
        ( ballX, ballY ) =
            ball.position

        ( _, ballVy ) =
            ball.velocity

        ( paddleX, paddleY ) =
            paddle.position

        ballCollidedWithPaddle =
            (paddleY <= ballY + ball.height)
                && (paddleX <= ballX && ballX <= paddleX + paddle.width)
                && (ballVy > 0)
    in
    if ballCollidedWithPaddle then
        Just paddle

    else
        Nothing


getPaddleHitByBallDistanceFromCenter : Float -> Ball -> Paddle -> Float
getPaddleHitByBallDistanceFromCenter scale ball paddle =
    case ballHitPaddle ball paddle of
        Just paddle_ ->
            let
                ballCenter =
                    Util.Vector.getX ball.position + ball.width / 2

                paddleCenter =
                    Util.Vector.getX paddle.position + paddle_.width / 2
            in
            (ballCenter - paddleCenter) * scale

        Nothing ->
            0



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


viewLives : Int -> Svg msg
viewLives lives =
    let
        ( width, height ) =
            ( 60, 10 )

        offset =
            44
    in
    (lives - 1)
        |> List.range 0
        |> List.map
            (\index ->
                Svg.image
                    [ Svg.Attributes.xlinkHref "/images/pixel-paddle.png"
                    , Svg.Attributes.x <| String.fromInt <| index * offset
                    , Svg.Attributes.y <| String.fromFloat <| Breakout.Window.initialWindow.height - 20
                    , Svg.Attributes.width <| String.fromInt width
                    , Svg.Attributes.height <| String.fromInt height
                    ]
                    []
            )
        |> Svg.g []
