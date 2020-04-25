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
    , deltaTimes : List Time
    , gameState : GameState
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
    , deltaTimes = Util.Fps.initialDeltaTimes
    , gameState = StartingScreen
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
                paddleDirection =
                    Breakout.Paddle.playerKeyPressToDirection model.playerKeyPress
            in
            ( { model
                | ball = updateBall model.ball deltaTime
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


updateBall : Ball -> Time -> Ball
updateBall ball deltaTime =
    { ball
        | position =
            ball.velocity
                |> Breakout.Vector.scale deltaTime
                |> Breakout.Vector.add ball.position
    }


updateKeyPress : String -> Model -> Model
updateKeyPress key model =
    if Set.member key Util.Keyboard.validKeys then
        { model | playerKeyPress = Set.insert key model.playerKeyPress }

    else
        model


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
        -- our direction is determined by the angle of the party popper cone
        -- (about 47°) as well as it's width (about 60°). We use a normal
        -- distribution here so that most of the confetti will come out in the
        -- same place, with falloff to the sides. We want most of the confetti
        -- to show up in the center 30°, so the standard deviation of the
        -- distribution should be 15°.
        |> Particle.withDirection (Random.Float.normal (degrees 345) (degrees 15))
        |> Particle.withSpeed (Random.Float.normal 600 100)
        |> Particle.withGravity 980
        |> Particle.withDrag
            (\confetti ->
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
            Browser.Events.onAnimationFrameDelta <| handleAnimationFrames

        PlayingScreen ->
            Browser.Events.onAnimationFrameDelta <| handleAnimationFrames

        EndingScreen ->
            Browser.Events.onAnimationFrameDelta <| handleAnimationFrames


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


view : Model -> Document Msg
view model =
    { title = "\u{1F6F8} Breakout"
    , body = [ viewMain model ]
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

        -- turns out that opacity is pretty expensive for browsers to calculate,
        -- and will slow down our framerate if we change it too much. So while
        -- we *could* do this with, like, a bezier curve or something, we
        -- actually want to just keep it as stable as possible until we actually
        -- need to fade out at the end.
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
        , Breakout.Paddle.viewPaddle model.paddle
        , Breakout.Paddle.viewPaddleScore model.paddle.score window 10
        , Breakout.Ball.viewBall model.ball
        , Breakout.Ball.viewBallPath model.ballPath |> Svg.g []
        , Particle.System.view viewParticles [] model.particleSystem
        -- , Util.Fps.viewFps Util.Fps.On model.deltaTimes
        ]
