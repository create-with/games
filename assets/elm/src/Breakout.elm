module Breakout exposing
    ( Model
    , Msg(..)
    , init
    , subscriptions
    , update
    , view
    )

-- IMPORTS

import Breakout.Ball exposing (Ball, BallPath, ShowBallPath)
import Breakout.Brick exposing (Brick, Bricks)
import Breakout.Paddle exposing (Direction, Paddle)
import Breakout.Window exposing (Window, WindowEdge)
import Browser exposing (Document)
import Browser.Events
import Dict
import Html exposing (Html)
import Html.Attributes
import Json.Decode
import Json.Encode
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
import Util.Fps exposing (ShowFps, Time)
import Util.Keyboard exposing (Controls)
import Util.Ports
import Util.Sound exposing (PlayMusic)
import Util.Vector
import Util.View



-- MODEL


type GameState
    = StartingScreen
    | PlayingScreen
    | PauseScreen
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
    , currentBrick : Maybe Brick
    , deltaTimes : List Time
    , gameState : GameState
    , paddle : Paddle
    , particleSystem : System Confetti
    , playerKeyPress : Controls
    , playMusic : PlayMusic
    , showBallPath : ShowBallPath
    , showFps : ShowFps
    , window : Window
    }



-- INIT


initialModel : Model
initialModel =
    { ball = Breakout.Ball.initialBall
    , ballPath = Breakout.Ball.initialBallPath
    , bricks = Breakout.Brick.initialBricks
    , currentBrick = Nothing
    , deltaTimes = Util.Fps.initialDeltaTimes
    , gameState = StartingScreen
    , paddle = Breakout.Paddle.initialPaddle
    , particleSystem = Particle.System.init (Random.initialSeed 0)
    , playerKeyPress = Util.Keyboard.initialKeys
    , playMusic = Util.Sound.initialPlayMusic
    , showBallPath = Breakout.Ball.initialShowBallPath
    , showFps = Util.Fps.initialShowFps
    , window = Breakout.Window.initialWindow
    }


initialCommand : Cmd Msg
initialCommand =
    playMusicCommand Util.Sound.initialPlayMusic "music.wav"


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, initialCommand )



-- UPDATE


type Msg
    = BrowserAdvancedAnimationFrame Time
    | CollisionGeneratedRandomWindowShakePositions ( Float, Float )
    | Particles
    | ParticleMsg (Particle.System.Msg Confetti)
    | PlayerClickedPlayMusicRadioButton PlayMusic
    | PlayerClickedShowBallPathRadioButton ShowBallPath
    | PlayerClickedShowFpsRadioButton ShowFps
    | PlayerClickedWindow
    | PlayerPressedKeyDown String
    | PlayerReleasedKey String
    | ShakeCompleted


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BrowserAdvancedAnimationFrame deltaTime ->
            let
                brickHitByBall =
                    Breakout.Brick.getBrickHitByBall model.ball model.bricks

                paddleDirection =
                    Breakout.Paddle.playerKeyPressToDirection model.playerKeyPress

                paddleHitByBall =
                    Breakout.Paddle.ballHitPaddle model.ball model.paddle

                windowEdgeHitByBall =
                    Breakout.Window.getWindowEdgeHitByBall model.ball model.window
            in
            ( { model
                | ball = updateBall deltaTime brickHitByBall paddleHitByBall windowEdgeHitByBall model.ball
                , ballPath = updateBallPath model.ball model.ballPath windowEdgeHitByBall model
                , bricks = updateBricks model.ball model.bricks
                , deltaTimes = updateDeltaTimes model.showFps deltaTime model.deltaTimes
                , gameState = updateGameState model.gameState model
                , paddle = updatePaddle paddleDirection brickHitByBall windowEdgeHitByBall model.window deltaTime model.paddle
              }
            , case brickHitByBall of
                Just _ ->
                    Cmd.batch [ generateRandomWindowShake, Process.sleep 0 |> Task.perform (\_ -> Particles) ]

                Nothing ->
                    Cmd.none
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
            ( { model | particleSystem = Particle.System.burst (Random.list 12 (particleAt x y)) model.particleSystem }
            , Cmd.none
            )

        ParticleMsg particleMsg ->
            ( { model | particleSystem = Particle.System.update particleMsg model.particleSystem }
            , Cmd.none
            )

        PlayerClickedPlayMusicRadioButton playMusicValue ->
            ( { model | playMusic = playMusicValue }, playMusicCommand playMusicValue "music.wav" )

        PlayerClickedShowBallPathRadioButton showBallPathValue ->
            ( { model | showBallPath = showBallPathValue }, Cmd.none )

        PlayerClickedShowFpsRadioButton showFpsValue ->
            ( { model | showFps = showFpsValue }, Cmd.none )

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

                        PauseScreen ->
                            ( { model | gameState = updateGameState PlayingScreen model }, Cmd.none )

                        EndingScreen ->
                            ( initialModel, Cmd.none )

                "Escape" ->
                    ( { model | gameState = updateGameState PauseScreen model }, Cmd.none )

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


updateBall : Time -> Maybe Brick -> Maybe Paddle -> Maybe WindowEdge -> Ball -> Ball
updateBall deltaTime maybeBrick maybePaddle maybeWindowEdge ball =
    ball
        |> handleBrickCollision maybeBrick
        |> handlePaddleCollision maybePaddle
        |> handleWindowCollision maybeWindowEdge
        |> handleBallUpdate deltaTime


handleBrickCollision : Maybe Brick -> Ball -> Ball
handleBrickCollision maybeBrick ball =
    case maybeBrick of
        Just _ ->
            -- NAIVE VELOCITY CHANGE
            { ball
                | velocity =
                    ( Util.Vector.getX ball.velocity
                    , negate <| Util.Vector.getY ball.velocity
                    )
            }

        Nothing ->
            ball


handlePaddleCollision : Maybe Paddle -> Ball -> Ball
handlePaddleCollision maybePaddle ball =
    let
        amountToChangeBallAngle =
            18.0

        amountToChangeBallSpeed =
            5.0
    in
    case maybePaddle of
        Just paddle ->
            { ball
                | velocity =
                    ( Breakout.Paddle.getPaddleHitByBallDistanceFromCenter amountToChangeBallAngle ball paddle
                    , negate <| Util.Vector.getY ball.velocity + amountToChangeBallSpeed
                    )
            }

        Nothing ->
            ball


handleBallUpdate : Time -> Ball -> Ball
handleBallUpdate deltaTime ball =
    { ball
        | position =
            ball.velocity
                |> Util.Vector.scale deltaTime
                |> Util.Vector.add ball.position
    }


handleWindowCollision : Maybe WindowEdge -> Ball -> Ball
handleWindowCollision maybeWindowEdge ball =
    let
        ( x, y ) =
            ball.position

        ( vx, vy ) =
            ball.velocity
    in
    case maybeWindowEdge of
        Just Breakout.Window.Bottom ->
            { ball
                | position = initialModel.ball.position
                , velocity = ( 0, 0 )
            }

        Just Breakout.Window.Left ->
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

        Just Breakout.Window.Right ->
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

        Just Breakout.Window.Top ->
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

        Nothing ->
            ball


updateBallPath : Ball -> BallPath -> Maybe WindowEdge -> Model -> BallPath
updateBallPath ball ballPath maybeWindowEdge { showBallPath } =
    case showBallPath of
        Breakout.Ball.Off ->
            []

        Breakout.Ball.On ->
            case maybeWindowEdge of
                Just Breakout.Window.Left ->
                    []

                Just Breakout.Window.Right ->
                    []

                _ ->
                    if ball.position /= Breakout.Ball.initialBall.position then
                        List.take 40 <| ball :: ballPath

                    else
                        []


updateBricks : Ball -> Bricks -> Bricks
updateBricks ball bricks =
    bricks
        |> Breakout.Brick.filterDestroyedBricks
        |> Dict.map (Breakout.Brick.incrementBrickHitCount ball)


updateDeltaTimes : ShowFps -> Time -> List Time -> List Time
updateDeltaTimes showFps deltaTime deltaTimes =
    case showFps of
        Util.Fps.Off ->
            deltaTimes

        Util.Fps.On ->
            List.take 50 (deltaTime :: deltaTimes)


updateGameState : GameState -> Model -> GameState
updateGameState gameState model =
    if (gameState == PlayingScreen && model.paddle.lives == 0) || Dict.isEmpty model.bricks then
        EndingScreen

    else
        gameState


updateKeyPress : String -> Model -> Model
updateKeyPress key model =
    if Set.member key Util.Keyboard.validKeys then
        { model | playerKeyPress = Set.insert key model.playerKeyPress }

    else
        model


updatePaddle : Maybe Direction -> Maybe Brick -> Maybe WindowEdge -> Window -> Time -> Paddle -> Paddle
updatePaddle maybeDirection maybeBrick maybeWindowEdge window deltaTime paddle =
    paddle
        |> Breakout.Paddle.updatePaddle maybeDirection deltaTime
        |> Breakout.Paddle.keepPaddleWithinWindow window
        |> Breakout.Paddle.updateScore maybeBrick
        |> Breakout.Paddle.updateLives maybeWindowEdge



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


playMusicCommand : PlayMusic -> String -> Cmd Msg
playMusicCommand playMusic soundFile =
    Util.Ports.playMusic <|
        Json.Encode.object
            [ ( "play", Json.Encode.bool <| Util.Sound.playMusicToBool playMusic )
            , ( "soundFile", Json.Encode.string soundFile )
            ]



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

        PauseScreen ->
            Sub.none

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
    , body = List.map (Html.map msg) [ viewMain model, Util.View.footer ]
    }


viewMain : Model -> Html Msg
viewMain model =
    Html.main_ [ Html.Attributes.class "bg-blue-400 h-full p-8" ]
        [ viewHeader
        , viewGame model
        , viewInformation model
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
        [ Html.h1 [ Html.Attributes.class "font-black text-5xl" ]
            [ Html.text "Breakout" ]
        ]


viewGame : Model -> Html Msg
viewGame model =
    Html.section [ Html.Attributes.class "flex justify-center my-4" ]
        [ Html.div [ Html.Attributes.class "flex-shrink-0" ]
            [ viewSvg model.window model ]
        ]


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
        , Breakout.Paddle.viewPaddleScore model.paddle.score
        , Breakout.Paddle.viewLives model.paddle.lives
        , Breakout.Ball.viewBall model.ball
        , Breakout.Ball.viewBallPath model.ballPath |> Svg.g []
        , Particle.System.view viewParticles [] model.particleSystem
        , Util.Fps.viewFps Util.Fps.On model.deltaTimes
        ]



-- VIEW INFO


viewInformation : Model -> Html Msg
viewInformation model =
    Html.section []
        [ viewEndingScreen model.gameState model.bricks
        , viewPauseScreen model.gameState
        , viewInstructions
        , viewOptions model.showBallPath model.showFps model.playMusic
        ]


viewPauseScreen : GameState -> Html msg
viewPauseScreen gameState =
    case gameState of
        PauseScreen ->
            Html.div [ Html.Attributes.class "pt-4 text-center" ]
                [ Html.h2 [ Html.Attributes.class "font-extrabold font-gray-800 pb-1 text-xl" ]
                    [ Html.text "Game Paused" ]
                , Html.p []
                    [ Html.text "⏯ Press the SPACEBAR key to continue the game." ]
                ]

        _ ->
            Html.span [] []



-- WINNER


viewEndingScreen : GameState -> Bricks -> Html msg
viewEndingScreen gameState bricks =
    case gameState of
        EndingScreen ->
            Html.div [ Html.Attributes.class "pt-4 text-center" ]
                [ Html.h2 [ Html.Attributes.class "font-extrabold font-gray-800 pb-1 text-xl" ]
                    [ if Dict.isEmpty bricks then
                        Html.text "Alas, you've won!"

                      else
                        Html.text "Game Over!"
                    ]
                , Html.p []
                    [ Html.text "🔁 Press the SPACEBAR key to reset the game." ]
                ]

        _ ->
            Html.span [] []



-- INSTRUCTIONS


viewInstructions : Html msg
viewInstructions =
    Html.div [ Html.Attributes.class "pt-4" ]
        [ Html.h2 [ Html.Attributes.class "font-extrabold font-gray-800 pb-1 text-center text-xl" ]
            [ Html.text "Instructions" ]
        , Html.div [ Html.Attributes.class "flex justify-center" ]
            [ Html.ul [ Html.Attributes.class "leading-relaxed list-disc list-inside mx-3" ]
                [ Html.li [] [ Html.text "\u{1F6F8} Press the SPACEBAR key to serve the ball." ]
                , Html.li [] [ Html.text "⌨️ Use the arrow keys to move the paddle." ]
                , Html.li [] [ Html.text "⏸ Press the ESCAPE key if you need to pause the game." ]
                , Html.li [] [ Html.text "🏆 Break all the bricks to win!" ]
                ]
            ]
        ]



-- OPTIONS


viewOptions : ShowBallPath -> ShowFps -> PlayMusic -> Html Msg
viewOptions showBallPath showFps playMusic =
    Html.div [ Html.Attributes.class "pt-4" ]
        [ Html.h2 [ Html.Attributes.class "font-extrabold font-gray-800 pb-1 text-center text-xl" ]
            [ Html.text "Options" ]
        , Html.form [ Html.Attributes.class "flex justify-center" ]
            [ Html.ul [ Html.Attributes.class "leading-relaxed list-disc list-inside mx-3" ]
                [ Html.li [] [ viewShowBallPathOptions showBallPath ]
                , Html.li [] [ viewShowFpsOptions showFps ]
                , Html.li [] [ viewPlayMusicOptions playMusic ]
                ]
            ]
        ]


viewShowBallPathOptions : ShowBallPath -> Html Msg
viewShowBallPathOptions showBallPath =
    Html.fieldset [ Html.Attributes.class "inline" ]
        [ Html.span [ Html.Attributes.class "mr-3" ]
            [ Html.text "Show ball path history:" ]
        , Util.View.radioButton Breakout.Ball.Off showBallPath Breakout.Ball.showBallPathToString PlayerClickedShowBallPathRadioButton
        , Util.View.radioButton Breakout.Ball.On showBallPath Breakout.Ball.showBallPathToString PlayerClickedShowBallPathRadioButton
        ]


viewShowFpsOptions : ShowFps -> Html Msg
viewShowFpsOptions showFps =
    Html.fieldset [ Html.Attributes.class "inline" ]
        [ Html.span [ Html.Attributes.class "mr-3" ]
            [ Html.text "Show FPS meter:" ]
        , Util.View.radioButton Util.Fps.Off showFps Util.Fps.showFpsToString PlayerClickedShowFpsRadioButton
        , Util.View.radioButton Util.Fps.On showFps Util.Fps.showFpsToString PlayerClickedShowFpsRadioButton
        ]


viewPlayMusicOptions : PlayMusic -> Html Msg
viewPlayMusicOptions playMusic =
    Html.fieldset [ Html.Attributes.class "inline" ]
        [ Html.span [ Html.Attributes.class "mr-3" ]
            [ Html.text "Play Music:" ]
        , Util.View.radioButton Util.Sound.Off playMusic Util.Sound.playMusicToString PlayerClickedPlayMusicRadioButton
        , Util.View.radioButton Util.Sound.On playMusic Util.Sound.playMusicToString PlayerClickedPlayMusicRadioButton
        ]
