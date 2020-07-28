module Breakout exposing
    ( Model
    , Msg(..)
    , init
    , subscriptions
    , update
    , view
    )

-- IMPORTS

import Breakout.Ball exposing (Ball, BallPath)
import Breakout.Brick exposing (Brick, Bricks, State)
import Breakout.Paddle exposing (Direction, Paddle)
import Breakout.Vector
import Breakout.Window exposing (Window, WindowEdge)
import Browser exposing (Document)
import Browser.Events
import Html exposing (Html)
import Html.Attributes
import Json.Decode
import Particle exposing (Particle)
import Particle.System exposing (System)
import Process
import Random exposing (Generator)
import Random.Extra
import Random.Float
import Set
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Task
import Util.Fps exposing (Time)
import Util.Keyboard exposing (Controls)



-- MODEL


type GameState
    = StartingScreen
    | PlayingScreen
    | EndingScreen


type Confetti
    = Dot
        { color : Color
        , rotations : Float
        , rotationOffset : Float
        }


type Color
    = Red
    | Pink
    | Yellow
    | Green
    | Blue


type alias Model =
    { ball : Ball
    , ballPath : BallPath
    , bricks : Bricks
    , deltaTimes : List Time
    , gameState : GameState
    , lives : Int
    , paddle : Paddle
    , particleSystem : System Confetti
    , playerKeyPress : Controls
    , window : Window
    }



-- INIT


initialModel : Model
initialModel =
    { ball = Breakout.Ball.initialBall
    , ballPath = Breakout.Ball.initialBallPath
    , bricks = Breakout.Brick.initialBricks
    , deltaTimes = Util.Fps.initialDeltaTimes
    , gameState = StartingScreen
    , lives = 3
    , paddle = Breakout.Paddle.initialPaddle
    , particleSystem = Particle.System.init (Random.initialSeed 0)
    , playerKeyPress = Util.Keyboard.initialKeys
    , window = Breakout.Window.initialWindow
    }


initialCommand : Cmd Msg
initialCommand =
    Cmd.none


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, initialCommand )



-- UPDATE


type Msg
    = BrowserAdvancedAnimationFrame Time
    | CollisionGeneratedRandomWindowShakePositions ( Float, Float )
    | Particles
    | ParticleMsg (Particle.System.Msg Confetti)
    | PlayerClickedWindow
    | PlayerPressedKeyDown String
    | PlayerReleasedKey String
    | ShakeCompleted


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BrowserAdvancedAnimationFrame deltaTime ->
            let
                bricksHitByBall : List Brick
                bricksHitByBall =
                    List.filterMap (Breakout.Brick.getBrickHitByBall model.ball) model.bricks

                paddleDirection =
                    Breakout.Paddle.playerKeyPressToDirection model.playerKeyPress

                paddleHitByBall =
                    Breakout.Paddle.ballHitPaddle model.ball model.paddle

                windowEdgeHitByBall =
                    Breakout.Window.getWindowEdgeHitByBall model.ball model.window
            in
            ( { model
                | ball = updateBall model.ball paddleHitByBall windowEdgeHitByBall deltaTime
                , bricks = updateBricks bricksHitByBall model.bricks
                , gameState = updateGameState model.gameState model
                , lives = updateLives model.lives windowEdgeHitByBall
                , paddle = updatePaddle model.paddle paddleDirection model.window deltaTime
              }
            , Cmd.none
            )

        CollisionGeneratedRandomWindowShakePositions ( randomX, randomY ) ->
            let
                shakeyness =
                    1.5
            in
            ( { model | window = shakeWindow randomX randomY shakeyness model.window }, sleepShake )

        Particles ->
            let
                ( x, y ) =
                    model.ball.position
            in
            ( { model | particleSystem = Particle.System.burst (Random.list 25 (particleAt x y)) model.particleSystem }
            , Cmd.none
            )

        ParticleMsg particleMsg ->
            ( { model | particleSystem = Particle.System.update particleMsg model.particleSystem }
            , Cmd.none
            )

        PlayerClickedWindow ->
            ( model, Cmd.batch [ generateRandomWindowShake, Process.sleep 0 |> Task.perform (\_ -> Particles) ] )

        PlayerPressedKeyDown key ->
            case key of
                " " ->
                    case model.gameState of
                        StartingScreen ->
                            ( { model | gameState = updateGameState PlayingScreen model }, Cmd.none )

                        PlayingScreen ->
                            let
                                setBallInMotion ball =
                                    { ball | velocity = initialModel.ball.velocity }
                            in
                            ( { model | ball = setBallInMotion model.ball }, Cmd.none )

                        EndingScreen ->
                            ( { model | gameState = updateGameState StartingScreen initialModel }, Cmd.none )

                _ ->
                    ( updateKeyPress key model, Cmd.none )

        PlayerReleasedKey _ ->
            ( { model | playerKeyPress = Set.empty }, Cmd.none )

        ShakeCompleted ->
            ( { model | window = Breakout.Window.initialWindow }, Cmd.none )



-- UPDATES


shakeWindow : Float -> Float -> Float -> Window -> Window
shakeWindow x y scale window =
    { window
        | x = x * scale
        , y = y * scale
    }


updateBall : Ball -> Bool -> Maybe WindowEdge -> Time -> Ball
updateBall ball paddleHit maybeWindowEdge deltaTime =
    let
        ( x, y ) =
            ball.position

        ( vx, vy ) =
            ball.velocity

        ( initialVx, _ ) =
            Breakout.Ball.initialBall.velocity
    in
    case ( paddleHit, maybeWindowEdge ) of
        ( True, _ ) ->
            { ball | velocity = ( vx, negate vy ) }

        ( False, Just edge ) ->
            case edge of
                Breakout.Window.Bottom ->
                    { ball
                        | position = initialModel.ball.position
                        , velocity = ( 0, 0 )
                    }

                Breakout.Window.Left ->
                    case compare 0 vy of
                        LT ->
                            { ball
                                | position = ( x + ball.width / 2, y + ball.height / 2 )
                                , velocity = ( negate vx, vy )
                            }

                        GT ->
                            { ball
                                | position = ( x + ball.width / 2, y - ball.height / 2 )
                                , velocity = ( negate vx, vy )
                            }

                        EQ ->
                            ball

                Breakout.Window.Right ->
                    case compare 0 vy of
                        LT ->
                            { ball
                                | position = ( x - ball.width / 2, y )
                                , velocity = ( negate vx, vy )
                            }

                        GT ->
                            { ball
                                | position = ( x - ball.width / 2, y )
                                , velocity = ( negate vx, vy )
                            }

                        EQ ->
                            ball

                Breakout.Window.Top ->
                    case compare 0 vx of
                        LT ->
                            { ball
                                | position = ( x + ball.width / 2, y + ball.height / 2 )
                                , velocity = ( vx, negate vy )
                            }

                        GT ->
                            { ball
                                | position = ( x - ball.width / 2, y + ball.height / 2 )
                                , velocity = ( vx, negate vy )
                            }

                        EQ ->
                            ball

        ( _, Nothing ) ->
            { ball
                | position =
                    ball.velocity
                        |> Breakout.Vector.scale deltaTime
                        |> Breakout.Vector.add ball.position
            }


updateBricks : List Brick -> List Brick -> List Brick
updateBricks bricksHitByBall bricks =
    List.map (Breakout.Brick.hideBrickHitByBall bricksHitByBall) bricks


updateGameState : GameState -> Model -> GameState
updateGameState gameState model =
    if gameState == PlayingScreen && model.lives == 0 then
        EndingScreen

    else
        gameState


updateKeyPress : String -> Model -> Model
updateKeyPress key model =
    if Set.member key Util.Keyboard.validKeys then
        { model | playerKeyPress = Set.insert key model.playerKeyPress }

    else
        model


updateLives : Int -> Maybe WindowEdge -> Int
updateLives lives maybeWindowEdge =
    case maybeWindowEdge of
        Just Breakout.Window.Bottom ->
            clamp 0 lives <| lives - 1

        _ ->
            lives


updatePaddle : Paddle -> Maybe Direction -> Window -> Time -> Paddle
updatePaddle paddle maybeDirection window deltaTime =
    paddle
        |> Breakout.Paddle.updatePaddle maybeDirection deltaTime
        |> Breakout.Paddle.keepPaddleWithinWindow window



-- COMMANDS


randomWindowShakeGenerator : Generator ( Float, Float )
randomWindowShakeGenerator =
    Random.pair (Random.float -16 16) (Random.float -16 16)


generateRandomWindowShake : Cmd Msg
generateRandomWindowShake =
    Random.generate CollisionGeneratedRandomWindowShakePositions randomWindowShakeGenerator


sleepShake : Cmd Msg
sleepShake =
    Process.sleep 10
        |> Task.perform (\_ -> ShakeCompleted)


dotGenerator : Generator Confetti
dotGenerator =
    Random.map3
        (\color rotations rotationOffset ->
            Dot
                { color = color
                , rotations = rotations
                , rotationOffset = rotationOffset
                }
        )
        (Random.weighted
            ( 1 / 5, Red )
            [ ( 1 / 5, Pink )
            , ( 1 / 5, Yellow )
            , ( 2 / 5, Green )
            , ( 2 / 5, Blue )
            ]
        )
        (Random.Float.normal 1 1)
        (Random.float 0 1)


confettiGenerator : Generator Confetti
confettiGenerator =
    Random.Extra.frequency ( 5 / 8, dotGenerator ) []


particleAt : Float -> Float -> Generator (Particle Confetti)
particleAt x y =
    Particle.init confettiGenerator
        |> Particle.withLifetime (Random.Float.normal 1.5 0.25)
        |> Particle.withLocation (Random.constant { x = x, y = y })
        |> Particle.withDirection (Random.Float.normal (degrees 345) (degrees 15))
        |> Particle.withSpeed (Random.Float.normal 600 100)
        |> Particle.withGravity 980
        |> Particle.withDrag
            (\_ ->
                { density = 0.001226
                , coefficient = 1.15
                , area = 1
                }
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ browserAnimationSubscription model.gameState
        , keyDownSubscription
        , keyUpSubscription
        , particleSystemSubscription model.particleSystem
        ]


browserAnimationSubscription : GameState -> Sub Msg
browserAnimationSubscription gameState =
    case gameState of
        StartingScreen ->
            Sub.none

        PlayingScreen ->
            Browser.Events.onAnimationFrameDelta <| handleAnimationFrames

        EndingScreen ->
            Sub.none


handleAnimationFrames : Time -> Msg
handleAnimationFrames milliseconds =
    BrowserAdvancedAnimationFrame <| milliseconds / 1000


keyDownSubscription : Sub Msg
keyDownSubscription =
    Browser.Events.onKeyDown <| Json.Decode.map PlayerPressedKeyDown <| Util.Keyboard.keyDecoder


keyUpSubscription : Sub Msg
keyUpSubscription =
    Browser.Events.onKeyUp <| Json.Decode.map PlayerReleasedKey <| Util.Keyboard.keyDecoder


particleSystemSubscription : System Confetti -> Sub Msg
particleSystemSubscription particleSystem =
    Particle.System.sub [] ParticleMsg particleSystem



-- VIEW


view : (Msg -> msg) -> Model -> Document msg
view msg model =
    { title = "\u{1F6F8} Breakout"
    , body = List.map (Html.map msg) [ viewMain model ]
    }


viewMain : Model -> Html Msg
viewMain model =
    Html.main_ [ Html.Attributes.class "bg-blue-400 h-full p-8" ]
        [ viewHeader
        , viewGame model
        ]


viewParticles : Particle Confetti -> Svg msg
viewParticles particle =
    let
        lifetime =
            Particle.lifetimePercent particle

        opacity =
            if lifetime < 0.1 then
                lifetime * 10

            else
                1
    in
    case Particle.data particle of
        Dot { color, rotationOffset, rotations } ->
            Svg.rect
                [ Svg.Attributes.width "10px"
                , Svg.Attributes.height "10px"
                , Svg.Attributes.x "-5px"
                , Svg.Attributes.y "-5px"
                , Svg.Attributes.rx "2px"
                , Svg.Attributes.ry "2px"
                , Svg.Attributes.fill (fill color)
                , Svg.Attributes.stroke "white"
                , Svg.Attributes.strokeWidth "4px"
                , Svg.Attributes.opacity <| String.fromFloat opacity
                , Svg.Attributes.transform <|
                    "rotate("
                        ++ String.fromFloat ((rotations * lifetime + rotationOffset) * 360)
                        ++ ")"
                ]
                []


fill : Color -> String
fill color =
    case color of
        Red ->
            "#D72D35"

        Pink ->
            "#F2298A"

        Yellow ->
            "#F2C618"

        Green ->
            "#2ACC42"

        Blue ->
            "#37CBE8"


viewHeader : Html msg
viewHeader =
    Html.header [ Html.Attributes.class "flex justify-center" ]
        [ Html.h1 [] [ Html.text "Breakout" ] ]


viewGame : Model -> Html Msg
viewGame model =
    Html.section [ Html.Attributes.class "flex justify-center my-4" ]
        [ Html.div [ Html.Attributes.class "flex-shrink-0" ]
            [ viewSvg model.window model ]
        ]


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


viewSvg : Window -> Model -> Svg Msg
viewSvg window model =
    let
        viewBoxString =
            [ window.x
            , window.y
            , window.width
            , window.height
            ]
                |> List.map String.fromFloat
                |> String.join " "
    in
    Svg.svg
        [ Svg.Attributes.viewBox viewBoxString
        , Svg.Attributes.width <| String.fromFloat window.width
        , Svg.Attributes.height <| String.fromFloat window.height
        , Svg.Events.onClick PlayerClickedWindow
        ]
        [ Breakout.Window.viewGameWindow window
        , Breakout.Brick.viewBricks model.bricks
        , Breakout.Paddle.viewPaddle model.paddle
        , Breakout.Paddle.viewPaddleScore model.paddle.score window 10
        , viewLives model.lives
        , Breakout.Ball.viewBall model.ball
        , Breakout.Ball.viewBallPath model.ballPath |> Svg.g []
        , Particle.System.view viewParticles [] model.particleSystem

        -- , Util.Fps.viewFps Util.Fps.On model.deltaTimes
        ]
