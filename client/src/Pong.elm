module Pong exposing
    ( Model
    , Msg(..)
    , init
    , subscriptions
    , update
    , view
    )

-- IMPORTS

import Browser.Events
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Json.Encode
import Keyboard exposing (Controls)
import Pong.Ball exposing (Ball, BallPath, ShowBallPath)
import Pong.Cabinet
import Pong.Fps exposing (DeltaTimes, ShowFps)
import Pong.Game exposing (DeltaTime, State, Winner, WinningScore)
import Pong.Paddle exposing (Direction, Paddle)
import Pong.Ports
import Pong.Window exposing (Window, WindowEdge)
import Random exposing (Generator)
import Set
import Svg exposing (Svg)
import Svg.Attributes



-- MODEL


type alias Model =
    { ball : Ball
    , ballPath : BallPath
    , deltaTimes : DeltaTimes
    , gameState : State
    , leftPaddle : Paddle
    , playerKeyPress : Controls
    , rightPaddle : Paddle
    , showBallPath : ShowBallPath
    , showFps : ShowFps
    , winner : Winner
    , winningScore : WinningScore
    }



-- INIT


initialModel : Model
initialModel =
    { ball = Pong.Ball.initialBall
    , ballPath = Pong.Ball.initialBallPath
    , deltaTimes = Pong.Fps.initialDeltaTimes
    , gameState = Pong.Game.initialState
    , leftPaddle = Pong.Paddle.initialLeftPaddle
    , playerKeyPress = Keyboard.initialKeys
    , rightPaddle = Pong.Paddle.initialRightPaddle
    , showBallPath = Pong.Ball.initialShowBallPath
    , showFps = Pong.Fps.initialShowFps
    , winner = Pong.Game.initialWinner
    , winningScore = Pong.Game.initialWinningScore
    }


initialCommand : Cmd Msg
initialCommand =
    Cmd.none


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, initialCommand )



-- UPDATE


type Msg
    = BrowserAdvancedAnimationFrame DeltaTime
    | CollisionGeneratedRandomBallYPositionAndYVelocity ( Int, Float )
    | PlayerClickedShowBallPathRadioButton ShowBallPath
    | PlayerClickedShowFpsRadioButton ShowFps
    | PlayerClickedWinningScoreRadioButton WinningScore
    | PlayerPressedKeyDown String
    | PlayerReleasedKey String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BrowserAdvancedAnimationFrame deltaTime ->
            let
                getWindowEdgeHitByBall =
                    Pong.Window.getWindowEdgeHitByBall model.ball Pong.Window.globalWindow

                getPaddleHitByBall =
                    Pong.Paddle.getPaddleHitByBall model.ball model.leftPaddle model.rightPaddle

                getWinner =
                    Pong.Game.getWinner model.leftPaddle model.rightPaddle model.winningScore

                leftPaddleDirection =
                    Pong.Paddle.playerKeyPressToDirection model.playerKeyPress
            in
            model
                |> updateBall model.ball getPaddleHitByBall getWindowEdgeHitByBall deltaTime
                |> updateBallPath model.ball model.ballPath getWindowEdgeHitByBall
                |> updateDeltaTimes model.showFps deltaTime
                |> updatePaddle model.leftPaddle leftPaddleDirection model.ball Pong.Window.globalWindow deltaTime
                |> updatePaddle model.rightPaddle Nothing model.ball Pong.Window.globalWindow deltaTime
                |> updatePaddleScores getWindowEdgeHitByBall
                |> updateWinner getWinner
                |> updateGameState model.gameState getWinner
                |> addCommand getPaddleHitByBall getWindowEdgeHitByBall

        CollisionGeneratedRandomBallYPositionAndYVelocity ( randomYPosition, randomYVelocity ) ->
            ( { model | ball = updateBallWithRandomness randomYPosition randomYVelocity model.ball }, Cmd.none )

        PlayerClickedShowBallPathRadioButton showBallPathValue ->
            ( { model | showBallPath = showBallPathValue }, Cmd.none )

        PlayerClickedShowFpsRadioButton showFpsValue ->
            ( { model | showFps = showFpsValue }, Cmd.none )

        PlayerClickedWinningScoreRadioButton winningScoreValue ->
            ( { model | winningScore = winningScoreValue }, Cmd.none )

        PlayerPressedKeyDown key ->
            case key of
                " " ->
                    case model.gameState of
                        Pong.Game.StartingScreen ->
                            ( updateGameState Pong.Game.PlayingScreen Nothing model, Cmd.none )

                        Pong.Game.PlayingScreen ->
                            ( model, Cmd.none )

                        Pong.Game.EndingScreen ->
                            ( updateGameState Pong.Game.StartingScreen Nothing model, Cmd.none )

                _ ->
                    ( updateKeyPress key model, Cmd.none )

        PlayerReleasedKey _ ->
            ( { model | playerKeyPress = Set.empty }, Cmd.none )



-- UPDATES


updateBall : Ball -> Maybe Paddle -> Maybe WindowEdge -> DeltaTime -> Model -> Model
updateBall ball maybePaddle maybeWindowEdge deltaTime model =
    { model | ball = updateBallWithCollisions ball maybePaddle maybeWindowEdge deltaTime }


updateBallWithCollisions : Ball -> Maybe Paddle -> Maybe WindowEdge -> DeltaTime -> Ball
updateBallWithCollisions ball maybePaddle maybeWindowEdge deltaTime =
    let
        vy =
            case Pong.Paddle.getPaddleHitByBallDistanceFromCenter ball maybePaddle of
                Just distance ->
                    distance * 5

                Nothing ->
                    0
    in
    case ( maybePaddle, maybeWindowEdge ) of
        ( Just paddle, Nothing ) ->
            case paddle.id of
                Pong.Paddle.Left ->
                    { ball
                        | x = ball.x + ball.width
                        , vx = negate <| ball.vx - 50
                        , vy = toFloat vy
                    }

                Pong.Paddle.Right ->
                    { ball
                        | x = ball.x - ball.width
                        , vx = negate <| ball.vx + 50
                    }

        ( Nothing, Just edge ) ->
            case edge of
                Pong.Window.Bottom ->
                    { ball
                        | y = ball.y - ball.height
                        , vy = negate ball.vy
                    }

                Pong.Window.Left ->
                    { ball
                        | x = Pong.Ball.initialBall.x
                        , vx = negate Pong.Ball.initialBall.vx
                        , vy = Pong.Ball.initialBall.vy
                    }

                Pong.Window.Right ->
                    { ball
                        | x = Pong.Ball.initialBall.x
                        , vx = Pong.Ball.initialBall.vx
                        , vy = Pong.Ball.initialBall.vy
                    }

                Pong.Window.Top ->
                    { ball
                        | y = ball.y + ball.height
                        , vy = negate ball.vy
                    }

        ( Just paddle, Just _ ) ->
            case paddle.id of
                Pong.Paddle.Left ->
                    { ball
                        | x = ball.x + ball.width
                        , vx = negate <| ball.vx - 50
                    }

                Pong.Paddle.Right ->
                    { ball
                        | x = ball.x - ball.width
                        , vx = negate <| ball.vx + 50
                    }

        ( Nothing, Nothing ) ->
            { ball
                | x = round <| toFloat ball.x + ball.vx * deltaTime
                , y = round <| toFloat ball.y + ball.vy * deltaTime
            }


updateBallWithRandomness : Int -> Float -> Ball -> Ball
updateBallWithRandomness y vy ball =
    { ball
        | y = y
        , vy = vy
    }


updateBallPath : Ball -> BallPath -> Maybe WindowEdge -> Model -> Model
updateBallPath ball ballPath maybeWindowEdge model =
    case model.showBallPath of
        Pong.Ball.Off ->
            model

        Pong.Ball.On ->
            case maybeWindowEdge of
                Just Pong.Window.Left ->
                    { model | ballPath = [] }

                Just Pong.Window.Right ->
                    { model | ballPath = [] }

                _ ->
                    { model | ballPath = List.take 99 <| ball :: ballPath }


updateDeltaTimes : ShowFps -> DeltaTime -> Model -> Model
updateDeltaTimes showFps deltaTime model =
    case showFps of
        Pong.Fps.Off ->
            model

        Pong.Fps.On ->
            { model | deltaTimes = List.take 50 (deltaTime :: model.deltaTimes) }


updateGameState : State -> Maybe Paddle -> Model -> Model
updateGameState gameState maybePaddle model =
    case maybePaddle of
        Just _ ->
            { model | gameState = Pong.Game.EndingScreen }

        Nothing ->
            { model | gameState = gameState }


updateKeyPress : String -> Model -> Model
updateKeyPress key model =
    if Set.member key Keyboard.validKeys then
        { model | playerKeyPress = Set.insert key model.playerKeyPress }

    else
        model


updatePaddle : Paddle -> Maybe Direction -> Ball -> Window -> DeltaTime -> Model -> Model
updatePaddle paddle maybeDirection ball window deltaTime model =
    case paddle.id of
        Pong.Paddle.Left ->
            { model
                | leftPaddle =
                    paddle
                        |> Pong.Paddle.updateLeftPaddle maybeDirection ball deltaTime
                        |> Pong.Paddle.updateYWithinWindow window
            }

        Pong.Paddle.Right ->
            { model
                | rightPaddle =
                    paddle
                        |> Pong.Paddle.updateRightPaddle ball deltaTime
                        |> Pong.Paddle.updateYWithinWindow window
            }


updatePaddleScores : Maybe WindowEdge -> Model -> Model
updatePaddleScores maybeWindowEdge model =
    case maybeWindowEdge of
        Just Pong.Window.Left ->
            { model | rightPaddle = Pong.Paddle.updateScore model.rightPaddle }

        Just Pong.Window.Right ->
            { model | leftPaddle = Pong.Paddle.updateScore model.leftPaddle }

        _ ->
            model


updateWinner : Maybe Paddle -> Model -> Model
updateWinner maybePaddle model =
    { model | winner = maybePaddle }



-- COMMANDS


addCommand : Maybe Paddle -> Maybe WindowEdge -> Model -> ( Model, Cmd Msg )
addCommand maybePaddle maybeWindowEdge model =
    case ( maybePaddle, maybeWindowEdge ) of
        ( Just _, Nothing ) ->
            ( model, playSoundCommand "beep.wav" )

        ( Nothing, Just edge ) ->
            case edge of
                Pong.Window.Bottom ->
                    ( model, playSoundCommand "boop.wav" )

                Pong.Window.Left ->
                    ( model, generateRandomBallPosition )

                Pong.Window.Right ->
                    ( model, generateRandomBallPosition )

                Pong.Window.Top ->
                    ( model, playSoundCommand "boop.wav" )

        ( _, _ ) ->
            ( model, Cmd.none )


randomBallYPositionGenerator : Generator Int
randomBallYPositionGenerator =
    Random.int
        (Pong.Window.globalWindow.y + 100)
        (Pong.Window.globalWindow.height - 100)


randomBallYVelocityGenerator : Generator Float
randomBallYVelocityGenerator =
    Random.float -350.0 350.0


randomBallPositionAndVelocity : Generator ( Int, Float )
randomBallPositionAndVelocity =
    Random.pair randomBallYPositionGenerator randomBallYVelocityGenerator


generateRandomBallPosition : Cmd Msg
generateRandomBallPosition =
    Random.generate CollisionGeneratedRandomBallYPositionAndYVelocity randomBallPositionAndVelocity


playSoundCommand : String -> Cmd Msg
playSoundCommand soundFile =
    Pong.Ports.playSound <| Json.Encode.string soundFile



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ browserAnimationSubscription model.gameState
        , keyDownSubscription
        , keyUpSubscription
        ]


browserAnimationSubscription : State -> Sub Msg
browserAnimationSubscription gameState =
    case gameState of
        Pong.Game.StartingScreen ->
            Sub.none

        Pong.Game.PlayingScreen ->
            Browser.Events.onAnimationFrameDelta <| handleAnimationFrames

        Pong.Game.EndingScreen ->
            Sub.none


handleAnimationFrames : DeltaTime -> Msg
handleAnimationFrames milliseconds =
    BrowserAdvancedAnimationFrame <| milliseconds / 1000


keyDownSubscription : Sub Msg
keyDownSubscription =
    Browser.Events.onKeyDown <| Json.Decode.map PlayerPressedKeyDown <| Keyboard.keyDecoder


keyUpSubscription : Sub Msg
keyUpSubscription =
    Browser.Events.onKeyUp <| Json.Decode.map PlayerReleasedKey <| Keyboard.keyDecoder



-- VIEW


view : Model -> Html Msg
view model =
    Html.main_ [ Html.Attributes.class "bg-yellow-200 p-6" ]
        [ viewHeader
        , viewGameSection model
        , viewInformation model
        ]


viewHeader : Html msg
viewHeader =
    Html.header [ Html.Attributes.class "flex justify-center" ]
        [ Pong.Cabinet.logo ]


viewGameSection : Model -> Html Msg
viewGameSection model =
    Html.section [ Html.Attributes.class "flex justify-center my-4" ]
        [ Html.div [ Html.Attributes.class "flex-shrink-0" ]
            [ viewSvg Pong.Window.globalWindow model ]
        ]


viewSvg : Window -> Model -> Svg msg
viewSvg window model =
    let
        viewBoxString =
            [ window.x
            , window.y
            , window.width
            , window.height
            ]
                |> List.map String.fromInt
                |> String.join " "
    in
    Svg.svg
        [ Svg.Attributes.viewBox viewBoxString
        , Svg.Attributes.width <| String.fromInt window.width
        , Svg.Attributes.height <| String.fromInt window.height
        ]
        [ Pong.Window.viewGameWindow window
        , Pong.Window.viewNet window
        , Pong.Paddle.viewPaddleScore model.leftPaddle.score window -200
        , Pong.Paddle.viewPaddleScore model.rightPaddle.score window 150
        , Pong.Paddle.viewPaddle model.leftPaddle
        , Pong.Paddle.viewPaddle model.rightPaddle
        , Pong.Ball.viewBall model.ball
        , Pong.Fps.viewFps model.showFps model.deltaTimes
        , Pong.Ball.viewBallPath model.showBallPath model.ballPath |> Svg.g []
        ]


viewWinner : State -> Maybe Paddle -> Html msg
viewWinner gameState maybePaddle =
    case gameState of
        Pong.Game.StartingScreen ->
            Html.span [] []

        Pong.Game.PlayingScreen ->
            Html.span [] []

        Pong.Game.EndingScreen ->
            Html.div [ Html.Attributes.class "pt-2" ]
                [ Html.h2 [ Html.Attributes.class "font-bold font-gray-800 pb-1 text-xl" ]
                    [ Html.text "Winner!" ]
                , viewWinnerPaddle maybePaddle
                ]


viewWinnerPaddle : Maybe Paddle -> Html msg
viewWinnerPaddle maybePaddle =
    case maybePaddle of
        Just paddle ->
            Html.div []
                [ Html.p []
                    [ Html.text <| "\u{1F947} " ++ Pong.Paddle.paddleIdToString paddle.id ++ " paddle wins!" ]
                , Html.p []
                    [ Html.text "🆕 Press the SPACEBAR key to reset the game." ]
                ]

        Nothing ->
            Html.span [] []


viewInformation : Model -> Html Msg
viewInformation model =
    Html.section []
        [ viewWinner model.gameState model.winner
        , viewInstructions
        , viewOptions model.showBallPath model.showFps model.winningScore
        ]


viewInstructions : Html msg
viewInstructions =
    Html.div [ Html.Attributes.class "pt-2" ]
        [ Html.h2 [ Html.Attributes.class "font-bold font-gray-800 pb-1 text-xl" ]
            [ Html.text "Instructions" ]
        , Html.ul [ Html.Attributes.class "list-disc list-inside mx-3" ]
            [ Html.li [] [ Html.text "\u{1F3D3} Press the SPACEBAR key to serve the ball." ]
            , Html.li [] [ Html.text "⌨️ Use the arrow keys to move the left paddle." ]
            , Html.li [] [ Html.text "🏆 Avoid missing ball for high score." ]
            ]
        ]


viewOptions : ShowBallPath -> ShowFps -> WinningScore -> Html Msg
viewOptions showBallPath_ showFps winningScore =
    Html.div [ Html.Attributes.class "pt-2" ]
        [ Html.h2 [ Html.Attributes.class "font-bold font-gray-800 pb-1 text-xl" ]
            [ Html.text "Options" ]
        , Html.form []
            [ Html.ul [ Html.Attributes.class "list-disc list-inside mx-3" ]
                [ Html.li [] [ viewShowBallPathOptions showBallPath_ ]
                , Html.li [] [ viewShowFpsOptions showFps ]
                , Html.li [] [ viewWinningScoreOptions winningScore ]
                ]
            ]
        ]


viewShowBallPathOptions : ShowBallPath -> Html Msg
viewShowBallPathOptions showBallPath_ =
    Html.fieldset [ Html.Attributes.class "inline" ]
        [ Html.span [ Html.Attributes.class "mr-3" ]
            [ Html.text "Show ball path history:" ]
        , viewRadioButton Pong.Ball.Off showBallPath_ Pong.Ball.showBallPathToString PlayerClickedShowBallPathRadioButton
        , viewRadioButton Pong.Ball.On showBallPath_ Pong.Ball.showBallPathToString PlayerClickedShowBallPathRadioButton
        ]


viewShowFpsOptions : ShowFps -> Html Msg
viewShowFpsOptions showFps_ =
    Html.fieldset [ Html.Attributes.class "inline" ]
        [ Html.span [ Html.Attributes.class "mr-3" ]
            [ Html.text "Show FPS meter:" ]
        , viewRadioButton Pong.Fps.Off showFps_ Pong.Fps.showFpsToString PlayerClickedShowFpsRadioButton
        , viewRadioButton Pong.Fps.On showFps_ Pong.Fps.showFpsToString PlayerClickedShowFpsRadioButton
        ]


viewWinningScoreOptions : WinningScore -> Html Msg
viewWinningScoreOptions winningScore =
    Html.fieldset [ Html.Attributes.class "inline" ]
        [ Html.span [ Html.Attributes.class "mr-3" ]
            [ Html.text "Set winning score:" ]
        , viewRadioButton Pong.Game.Eleven winningScore Pong.Game.winningScoreToString PlayerClickedWinningScoreRadioButton
        , viewRadioButton Pong.Game.Fifteen winningScore Pong.Game.winningScoreToString PlayerClickedWinningScoreRadioButton
        ]



-- VIEW HELPERS


viewRadioButton : a -> a -> (a -> String) -> (a -> Msg) -> Html Msg
viewRadioButton type_ current toString msg =
    Html.label []
        [ Html.input
            [ Html.Attributes.checked <| current == type_
            , Html.Attributes.type_ "radio"
            , Html.Events.onClick <| msg type_
            ]
            []
        , Html.span [ Html.Attributes.class "px-1 text-xs" ]
            [ Html.text <| toString type_ ]
        ]
