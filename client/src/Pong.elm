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
import Pong.Fps exposing (DeltaTimes, ShowFps)
import Pong.Game exposing (DeltaTime, State, Winner, WinningScore)
import Pong.Paddle exposing (Direction, Paddle)
import Pong.Ports
import Pong.Window exposing (Window, WindowEdge)
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
                paddleHitByBall_ =
                    paddleHitByBall model.ball model.leftPaddle model.rightPaddle

                ballHitWindowEdge =
                    Pong.Window.ballHitEdge model.ball Pong.Window.globalWindow

                leftPaddleDirection =
                    Pong.Paddle.playerKeyPressToDirection model.playerKeyPress

                determineWinner_ =
                    determineWinner model.leftPaddle model.rightPaddle model.winningScore
            in
            model
                |> updateBall model.ball paddleHitByBall_ ballHitWindowEdge deltaTime
                |> updateBallPath model.ball model.ballPath
                |> updateDeltaTimes model.showFps deltaTime
                |> updatePaddle model.leftPaddle leftPaddleDirection model.ball Pong.Window.globalWindow deltaTime
                |> updatePaddle model.rightPaddle Nothing model.ball Pong.Window.globalWindow deltaTime
                |> updatePaddleScores ballHitWindowEdge
                |> updateWinner determineWinner_
                |> addCommand paddleHitByBall_ ballHitWindowEdge

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
                            ( updateGameState Pong.Game.PlayingScreen model, Cmd.none)

                        Pong.Game.PlayingScreen ->
                            ( model, Cmd.none )

                        Pong.Game.EndingScreen ->
                            ( updateGameState Pong.Game.StartingScreen model, Cmd.none)

                _ ->
                    ( updateKeyPress key model, Cmd.none )

        PlayerReleasedKey _ ->
            ( clearKeyPresses model, Cmd.none )



-- UPDATE PREDICATES


paddleHitByBall : Ball -> Paddle -> Paddle -> Maybe Paddle
paddleHitByBall ball leftPaddle rightPaddle =
    if ballHitLeftPaddle ball leftPaddle then
        Just leftPaddle

    else if ballHitRightPaddle ball rightPaddle then
        Just rightPaddle

    else
        Nothing


ballHitLeftPaddle : Ball -> Paddle -> Bool
ballHitLeftPaddle ball paddle =
    (paddle.y <= ball.y && ball.y <= paddle.y + paddle.height)
        && (paddle.x <= ball.x && ball.x <= paddle.x + paddle.width)


ballHitRightPaddle : Ball -> Paddle -> Bool
ballHitRightPaddle ball paddle =
    (paddle.y <= ball.y && ball.y <= paddle.y + paddle.height)
        && (paddle.x <= ball.x + ball.width && ball.x <= paddle.x + paddle.width)


determineWinner : Paddle -> Paddle -> WinningScore -> Maybe Paddle
determineWinner leftPaddle rightPaddle winningScore =
    if leftPaddle.score == Pong.Game.winningScoreToInt winningScore then
        Just leftPaddle

    else if rightPaddle.score == Pong.Game.winningScoreToInt winningScore then
        Just rightPaddle

    else
        Nothing



-- UPDATES


updateBall : Ball -> Maybe Paddle -> Maybe WindowEdge -> DeltaTime -> Model -> Model
updateBall ball maybePaddle maybeWindowEdge deltaTime model =
    { model | ball = updateBallWithCollisions ball maybePaddle maybeWindowEdge deltaTime }


updateBallWithCollisions : Ball -> Maybe Paddle -> Maybe WindowEdge -> DeltaTime -> Ball
updateBallWithCollisions ball maybePaddle maybeWindowEdge deltaTime =
    case ( maybePaddle, maybeWindowEdge ) of
        -- ball hit paddle, ball did not hit edge
        ( Just paddle, Nothing ) ->
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

        -- ball did not hit paddle, ball hit edge
        ( Nothing, Just edge ) ->
            case edge of
                Pong.Window.Bottom ->
                    { ball
                        | y = ball.y - ball.height
                        , vy = negate ball.vy
                    }

                Pong.Window.Left ->
                    Pong.Ball.initialBall

                Pong.Window.Right ->
                    Pong.Ball.initialBall

                Pong.Window.Top ->
                    { ball
                        | y = ball.y + ball.height
                        , vy = negate ball.vy
                    }

        -- ball hit paddle, ball hit edge (hadn't thought about this case)
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

        -- ball did not hit paddle, ball did not hit edge
        ( Nothing, Nothing ) ->
            { ball
                | x = round <| toFloat ball.x + ball.vx * deltaTime
                , y = round <| toFloat ball.y + ball.vy * deltaTime
            }


updateBallPath : Ball -> BallPath -> Model -> Model
updateBallPath ball ballPath model =
    case model.showBallPath of
        Pong.Ball.Off ->
            { model | ballPath = [] }

        Pong.Ball.On ->
            { model | ballPath = List.take 99 <| ball :: ballPath }


clearBallPath : Model -> Model
clearBallPath model =
    { model | ballPath = [] }


updateDeltaTimes : ShowFps -> DeltaTime -> Model -> Model
updateDeltaTimes showFps deltaTime model =
    case showFps of
        Pong.Fps.Off ->
            model

        Pong.Fps.On ->
            { model | deltaTimes = List.take 50 (deltaTime :: model.deltaTimes) }


updateGameState : State -> Model -> Model
updateGameState newGameState model =
    { model | gameState = newGameState }


updatePaddleScores : Maybe WindowEdge -> Model -> Model
updatePaddleScores maybeWindowEdge model =
    case maybeWindowEdge of
        Just Pong.Window.Left ->
            let
                paddle =
                    model.rightPaddle
            in
            { model | rightPaddle = Pong.Paddle.updateScore paddle }

        Just Pong.Window.Right ->
            let
                paddle =
                    model.leftPaddle
            in
            { model | leftPaddle = Pong.Paddle.updateScore paddle }

        _ ->
            model


clearKeyPresses : Model -> Model
clearKeyPresses model =
    { model | playerKeyPress = Set.empty }


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


updateWinner : Maybe Paddle -> Model -> Model
updateWinner winner model =
    { model | winner = winner }



-- COMMANDS


addCommand : Maybe Paddle -> Maybe WindowEdge -> Model -> ( Model, Cmd Msg )
addCommand maybePaddle maybeWindowEdge model =
    case ( maybePaddle, maybeWindowEdge ) of
        ( Just _, Nothing ) ->
            ( model, playSoundCommand "beep.wav" )

        ( Nothing, Just _ ) ->
            ( model, playSoundCommand "boop.wav" )

        ( _, _ ) ->
            ( model, Cmd.none )


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
    Html.main_ [ Html.Attributes.class "px-6" ]
        [ viewHeader
        , Html.section []
            [ case model.gameState of
                Pong.Game.StartingScreen ->
                    Html.div []
                        [ viewSvg Pong.Window.globalWindow model
                        , viewInstructions
                        , viewOptions model.showBallPath model.showFps model.winningScore
                        ]

                Pong.Game.PlayingScreen ->
                    Html.div []
                        [ viewSvg Pong.Window.globalWindow model
                        , viewInstructions
                        , viewOptions model.showBallPath model.showFps model.winningScore
                        ]

                Pong.Game.EndingScreen ->
                    Html.div []
                        [ viewSvg Pong.Window.globalWindow model
                        , viewWinner model.winner
                        , viewInstructions
                        , viewOptions model.showBallPath model.showFps model.winningScore
                        ]
            ]
        ]


viewHeader : Html msg
viewHeader =
    Html.header []
        [ Html.h1 [ Html.Attributes.class "font-black text-black text-5xl" ]
            [ Html.text "\u{1F3D3} Pong" ]
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
        ([ viewGameWindow window
         , viewNet window
         , viewPaddleScore model.leftPaddle.score window -200
         , viewPaddleScore model.rightPaddle.score window 150
         , viewPaddle model.leftPaddle
         , viewPaddle model.rightPaddle
         , viewBall model.ball
         , Pong.Fps.viewFps model.showFps model.deltaTimes
         ]
            ++ viewBallPath model.showBallPath model.ballPath
        )


viewGameWindow : Window -> Svg msg
viewGameWindow window =
    Svg.rect
        [ Svg.Attributes.fill window.backgroundColor
        , Svg.Attributes.x <| String.fromInt window.x
        , Svg.Attributes.y <| String.fromInt window.y
        , Svg.Attributes.width <| String.fromInt window.width
        , Svg.Attributes.height <| String.fromInt window.height
        ]
        []


viewNet : Window -> Svg msg
viewNet window =
    Svg.line
        [ Svg.Attributes.stroke "white"
        , Svg.Attributes.strokeDasharray "14, 14"
        , Svg.Attributes.strokeWidth "4"
        , Svg.Attributes.x1 <| String.fromInt <| (window.width // 2)
        , Svg.Attributes.x2 <| String.fromInt <| (window.width // 2)
        , Svg.Attributes.y1 <| String.fromInt window.y
        , Svg.Attributes.y2 <| String.fromInt window.height
        ]
        []


viewPaddleScore : Int -> Window -> Int -> Svg msg
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


viewPaddle : Paddle -> Svg msg
viewPaddle paddle =
    Svg.rect
        [ Svg.Attributes.fill paddle.color
        , Svg.Attributes.x <| String.fromInt paddle.x
        , Svg.Attributes.y <| String.fromInt paddle.y
        , Svg.Attributes.width <| String.fromInt paddle.width
        , Svg.Attributes.height <| String.fromInt paddle.height
        ]
        []


viewBall : Ball -> Svg msg
viewBall ball =
    Svg.rect
        [ Svg.Attributes.fill ball.color
        , Svg.Attributes.x <| String.fromInt ball.x
        , Svg.Attributes.y <| String.fromInt ball.y
        , Svg.Attributes.width <| String.fromInt ball.width
        , Svg.Attributes.height <| String.fromInt ball.height
        ]
        []


viewBallPath : ShowBallPath -> List Ball -> List (Svg msg)
viewBallPath showBallPath ballPath =
    case showBallPath of
        Pong.Ball.On ->
            List.indexedMap viewBallPathSegment ballPath

        Pong.Ball.Off ->
            []


viewBallPathSegment : Int -> Ball -> Svg msg
viewBallPathSegment index ball =
    Svg.rect
        [ Svg.Attributes.fillOpacity <| String.fromFloat <| 0.01 * toFloat (80 - index)
        , Svg.Attributes.fill "darkorange"
        , Svg.Attributes.x <| String.fromInt ball.x
        , Svg.Attributes.y <| String.fromInt ball.y
        , Svg.Attributes.width <| String.fromInt ball.width
        , Svg.Attributes.height <| String.fromInt ball.height
        ]
        []


viewWinner : Maybe Paddle -> Html msg
viewWinner maybePaddle =
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
