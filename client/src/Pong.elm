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
import Html
import Html.Attributes
import Html.Events
import Json.Decode
import Json.Encode
import Pong.Ball
import Pong.Fps
import Pong.Game
import Pong.Keyboard
import Pong.Paddle
import Pong.Ports
import Pong.Window
import Set
import Svg
import Svg.Attributes



-- MODEL


type alias DeltaTime =
    Float


type alias Model =
    { ball : Pong.Ball.Ball
    , ballPath : Pong.Ball.BallPath
    , deltaTimes : Pong.Fps.DeltaTimes
    , gameState : Pong.Game.State
    , leftPaddle : Pong.Paddle.Paddle
    , playerKeyPress : Set.Set String
    , rightPaddle : Pong.Paddle.Paddle
    , showBallPath : Pong.Ball.ShowBallPath
    , showFps : Pong.Fps.ShowFps
    , winner : Maybe Pong.Paddle.PaddleId
    , winningScore : Pong.Game.WinningScore
    }



-- INIT


initialModel : Model
initialModel =
    { ball = Pong.Ball.initialBall
    , ballPath = Pong.Ball.initialBallPath
    , deltaTimes = Pong.Fps.initialDeltaTimes
    , gameState = Pong.Game.initialState
    , leftPaddle = Pong.Paddle.initialLeftPaddle
    , playerKeyPress = Set.empty
    , rightPaddle = Pong.Paddle.initialRightPaddle
    , showBallPath = Pong.Ball.initialShowBallPath
    , showFps = Pong.Fps.initialShowFps
    , winner = Nothing
    , winningScore = Pong.Game.Eleven
    }


initialCommand : Cmd Msg
initialCommand =
    Cmd.none


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, initialCommand )



-- UPDATE


type Msg
    = BrowserAdvancedAnimationFrame Pong.Game.State DeltaTime
    | PlayerClickedShowBallPathRadioButton Pong.Ball.ShowBallPath
    | PlayerClickedShowFpsRadioButton Pong.Fps.ShowFps
    | PlayerClickedWinningScoreRadioButton Pong.Game.WinningScore
    | PlayerPressedKeyDown String
    | PlayerReleasedKey String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BrowserAdvancedAnimationFrame Pong.Game.StartingScreen _ ->
            if Pong.Keyboard.playerPressedSpacebarKey model.playerKeyPress then
                model
                    |> updateGameState Pong.Game.PlayingScreen
                    |> noCommand

            else
                model |> noCommand

        BrowserAdvancedAnimationFrame Pong.Game.PlayingScreen deltaTime ->
            if leftPaddleHasWinningScore model.leftPaddle.score model.winningScore then
                model
                    |> updateGameState Pong.Game.EndingScreen
                    |> updateWinner (Just model.leftPaddle.id)
                    |> noCommand

            else if rightPaddleHasWinningScore model.rightPaddle.score model.winningScore then
                model
                    |> updateGameState Pong.Game.EndingScreen
                    |> updateWinner (Just model.rightPaddle.id)
                    |> noCommand

            else if ballHitLeftPaddle model.ball model.leftPaddle then
                model
                    |> updateBall model.ball (Just model.leftPaddle) Nothing deltaTime
                    |> updateDeltaTimes deltaTime
                    |> playSoundCommand "beep.wav"

            else if ballHitRightPaddle model.ball model.rightPaddle then
                model
                    |> updateBall model.ball (Just model.rightPaddle) Nothing deltaTime
                    |> updateDeltaTimes deltaTime
                    |> playSoundCommand "beep.wav"

            else if Pong.Window.ballHitEdge model.ball Pong.Window.globalWindow == Just Pong.Window.Right then
                model
                    |> updateBall Pong.Ball.initialBall Nothing Nothing deltaTime
                    |> updateDeltaTimes deltaTime
                    |> clearBallPath
                    |> updatePaddleScore model.leftPaddle
                    |> noCommand

            else if Pong.Window.ballHitEdge model.ball Pong.Window.globalWindow == Just Pong.Window.Left then
                model
                    |> updateBall Pong.Ball.initialBall Nothing Nothing deltaTime
                    |> updateDeltaTimes deltaTime
                    |> clearBallPath
                    |> updatePaddleScore model.rightPaddle
                    |> noCommand

            else if Pong.Window.ballHitEdge model.ball Pong.Window.globalWindow == Just Pong.Window.Top then
                model
                    |> updateBall model.ball Nothing (Just Pong.Window.Top) deltaTime
                    |> updateDeltaTimes deltaTime
                    |> playSoundCommand "boop.wav"

            else if Pong.Window.ballHitEdge model.ball Pong.Window.globalWindow == Just Pong.Window.Bottom then
                model
                    |> updateBall model.ball Nothing (Just Pong.Window.Bottom) deltaTime
                    |> updateDeltaTimes deltaTime
                    |> playSoundCommand "boop.wav"

            else if Pong.Keyboard.playerPressedArrowUpKey model.playerKeyPress then
                model
                    |> updateBall model.ball Nothing Nothing deltaTime
                    |> updateBallPath model.ball model.ballPath
                    |> updateDeltaTimes deltaTime
                    |> updateLeftPaddle (movePaddleUp model.leftPaddle)
                    |> updateRightPaddle model.rightPaddle model.ball
                    |> noCommand

            else if Pong.Keyboard.playerPressedArrowDownKey model.playerKeyPress then
                model
                    |> updateBall model.ball Nothing Nothing deltaTime
                    |> updateBallPath model.ball model.ballPath
                    |> updateDeltaTimes deltaTime
                    |> updateLeftPaddle (movePaddleDown model.leftPaddle)
                    |> updateRightPaddle model.rightPaddle model.ball
                    |> noCommand

            else
                model
                    |> updateBall model.ball Nothing Nothing deltaTime
                    |> updateBallPath model.ball model.ballPath
                    |> updateDeltaTimes deltaTime
                    |> updateRightPaddle model.rightPaddle model.ball
                    |> noCommand

        BrowserAdvancedAnimationFrame Pong.Game.EndingScreen _ ->
            if Pong.Keyboard.playerPressedSpacebarKey model.playerKeyPress then
                initialModel |> noCommand

            else
                model |> noCommand

        PlayerClickedShowBallPathRadioButton showBallPathValue ->
            model
                |> updateShowBallPath showBallPathValue
                |> noCommand

        PlayerClickedShowFpsRadioButton showFpsValue ->
            model
                |> updateShowFps showFpsValue
                |> noCommand

        PlayerClickedWinningScoreRadioButton winningScoreValue ->
            model
                |> updateWinningScore winningScoreValue
                |> noCommand

        PlayerPressedKeyDown key ->
            model
                |> updateKeyPress key
                |> noCommand

        PlayerReleasedKey _ ->
            model
                |> clearKeyPresses
                |> noCommand



-- UPDATE PREDICATES


ballHitLeftPaddle : Pong.Ball.Ball -> Pong.Paddle.Paddle -> Bool
ballHitLeftPaddle ball paddle =
    (paddle.y < ball.y && ball.y < paddle.y + paddle.height)
        && (paddle.x < ball.x && ball.x < paddle.x + paddle.width)


ballHitRightPaddle : Pong.Ball.Ball -> Pong.Paddle.Paddle -> Bool
ballHitRightPaddle ball paddle =
    (paddle.y < ball.y && ball.y < paddle.y + paddle.height)
        && (paddle.x < ball.x + ball.width && ball.x < paddle.x + paddle.width)


leftPaddleHasWinningScore : Int -> Pong.Game.WinningScore -> Bool
leftPaddleHasWinningScore leftPaddleScore winningScore =
    leftPaddleScore == Pong.Game.winningScoreToInt winningScore


rightPaddleHasWinningScore : Int -> Pong.Game.WinningScore -> Bool
rightPaddleHasWinningScore rightPaddleScore winningScore =
    rightPaddleScore == Pong.Game.winningScoreToInt winningScore



-- UPDATES


updateBall : Pong.Ball.Ball -> Maybe Pong.Paddle.Paddle -> Maybe Pong.Window.WindowEdge -> DeltaTime -> Model -> Model
updateBall newBall maybePaddle maybeEdge deltaTime model =
    let
        applyUpdates ball =
            case ( maybePaddle, maybeEdge ) of
                -- ball hit paddle, ball did not hit edge
                ( Just paddle, Nothing ) ->
                    let
                        changeInSpeed =
                            50
                    in
                    case paddle.id of
                        Pong.Paddle.Left ->
                            { ball
                                | x = ball.x + ball.width
                                , vx = negate <| (ball.vx - changeInSpeed)
                            }

                        Pong.Paddle.Right ->
                            { ball
                                | x = ball.x - ball.width
                                , vx = negate <| (ball.vx + changeInSpeed)
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
                            ball

                        Pong.Window.Right ->
                            ball

                        Pong.Window.Top ->
                            { ball
                                | y = ball.y + ball.height
                                , vy = negate ball.vy
                            }

                -- ball hit paddle, ball hit edge (hadn't thought about this case)
                ( Just _, Just _ ) ->
                    { ball
                        | x = round <| toFloat ball.x + ball.vx * deltaTime
                        , y = round <| toFloat ball.y + ball.vy * deltaTime
                    }

                -- ball did not hit paddle, ball did not hit edge
                ( Nothing, Nothing ) ->
                    { ball
                        | x = round <| toFloat ball.x + ball.vx * deltaTime
                        , y = round <| toFloat ball.y + ball.vy * deltaTime
                    }
    in
    { model | ball = applyUpdates newBall }


updateBallPath : Pong.Ball.Ball -> Pong.Ball.BallPath -> Model -> Model
updateBallPath ball ballPath model =
    case model.showBallPath of
        Pong.Ball.Off ->
            { model | ballPath = [] }

        Pong.Ball.On ->
            { model | ballPath = List.take 99 <| ball :: ballPath }


clearBallPath : Model -> Model
clearBallPath model =
    { model | ballPath = [] }


updateDeltaTimes : DeltaTime -> Model -> Model
updateDeltaTimes deltaTime model =
    { model | deltaTimes = List.take 50 (deltaTime :: model.deltaTimes) }


updateGameState : Pong.Game.State -> Model -> Model
updateGameState newGameState model =
    { model | gameState = newGameState }


updatePaddleScore : Pong.Paddle.Paddle -> Model -> Model
updatePaddleScore paddle model =
    case paddle.id of
        Pong.Paddle.Left ->
            { model | leftPaddle = Pong.Paddle.updateScore paddle }

        Pong.Paddle.Right ->
            { model | rightPaddle = Pong.Paddle.updateScore paddle }


updateLeftPaddle : Pong.Paddle.Paddle -> Model -> Model
updateLeftPaddle newLeftPaddle model =
    { model | leftPaddle = newLeftPaddle }


updateRightPaddle : Pong.Paddle.Paddle -> Pong.Ball.Ball -> Model -> Model
updateRightPaddle newRightPaddle ball model =
    let
        updatePaddle paddle =
            if ball.y > paddle.y then
                { paddle | y = keepPaddleInWindow paddle (paddle.y + 10) Pong.Window.globalWindow }

            else if ball.y < paddle.y then
                { paddle | y = keepPaddleInWindow paddle (paddle.y - 10) Pong.Window.globalWindow }

            else
                paddle
    in
    { model | rightPaddle = updatePaddle newRightPaddle }


updateShowBallPath : Pong.Ball.ShowBallPath -> Model -> Model
updateShowBallPath newShowBallPath model =
    { model | showBallPath = newShowBallPath }


updateShowFps : Pong.Fps.ShowFps -> Model -> Model
updateShowFps newShowFps model =
    { model | showFps = newShowFps }


updateWinner : Maybe Pong.Paddle.PaddleId -> Model -> Model
updateWinner newWinner model =
    { model | winner = newWinner }


updateWinningScore : Pong.Game.WinningScore -> Model -> Model
updateWinningScore newWinningScore model =
    { model | winningScore = newWinningScore }



-- COMMANDS


noCommand : Model -> ( Model, Cmd Msg )
noCommand model =
    ( model, Cmd.none )


playSoundCommand : String -> Model -> ( Model, Cmd Msg )
playSoundCommand soundFile model =
    ( model, Pong.Ports.playSound <| Json.Encode.string soundFile )



-- UPDATE HELPERS


keepPaddleInWindow : Pong.Paddle.Paddle -> Int -> Pong.Window.Window -> Int
keepPaddleInWindow paddle paddleYPosition window =
    let
        topEdge =
            window.y

        bottomEdge =
            window.height - paddle.height
    in
    Basics.clamp topEdge bottomEdge paddleYPosition


movePaddleDown : Pong.Paddle.Paddle -> Pong.Paddle.Paddle
movePaddleDown paddle =
    { paddle | y = keepPaddleInWindow paddle (paddle.y + 10) Pong.Window.globalWindow }


movePaddleUp : Pong.Paddle.Paddle -> Pong.Paddle.Paddle
movePaddleUp paddle =
    { paddle | y = keepPaddleInWindow paddle (paddle.y - 10) Pong.Window.globalWindow }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ browserAnimationSubscription model.gameState
        , keyDownSubscription
        , keyUpSubscription
        ]


browserAnimationSubscription : Pong.Game.State -> Sub Msg
browserAnimationSubscription gameState =
    Browser.Events.onAnimationFrameDelta <| handleAnimationFrames gameState


handleAnimationFrames : Pong.Game.State -> DeltaTime -> Msg
handleAnimationFrames gameState milliseconds =
    BrowserAdvancedAnimationFrame gameState <| milliseconds / 1000


keyDownSubscription : Sub Msg
keyDownSubscription =
    Browser.Events.onKeyDown <| Json.Decode.map PlayerPressedKeyDown <| Pong.Keyboard.keyDecoder


keyUpSubscription : Sub Msg
keyUpSubscription =
    Browser.Events.onKeyUp <| Json.Decode.map PlayerReleasedKey <| Pong.Keyboard.keyDecoder


clearKeyPresses : Model -> Model
clearKeyPresses model =
    { model | playerKeyPress = Set.empty }


updateKeyPress : String -> Model -> Model
updateKeyPress key model =
    if Set.member key Pong.Keyboard.validKeys then
        { model | playerKeyPress = Set.insert key model.playerKeyPress }

    else
        model



-- VIEW


view : Model -> Html.Html Msg
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


viewHeader : Html.Html msg
viewHeader =
    Html.header []
        [ Html.h1 [ Html.Attributes.class "font-black text-black text-5xl" ]
            [ Html.text "\u{1F3D3} Pong" ]
        ]


viewSvg : Pong.Window.Window -> Model -> Svg.Svg msg
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


viewGameWindow : Pong.Window.Window -> Svg.Svg msg
viewGameWindow window =
    Svg.rect
        [ Svg.Attributes.fill window.backgroundColor
        , Svg.Attributes.x <| String.fromInt window.x
        , Svg.Attributes.y <| String.fromInt window.y
        , Svg.Attributes.width <| String.fromInt window.width
        , Svg.Attributes.height <| String.fromInt window.height
        ]
        []


viewNet : Pong.Window.Window -> Svg.Svg msg
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


viewPaddle : Pong.Paddle.Paddle -> Svg.Svg msg
viewPaddle paddle =
    Svg.rect
        [ Svg.Attributes.fill paddle.color
        , Svg.Attributes.x <| String.fromInt paddle.x
        , Svg.Attributes.y <| String.fromInt paddle.y
        , Svg.Attributes.width <| String.fromInt paddle.width
        , Svg.Attributes.height <| String.fromInt paddle.height
        ]
        []


viewBall : Pong.Ball.Ball -> Svg.Svg msg
viewBall ball =
    Svg.rect
        [ Svg.Attributes.fill ball.color
        , Svg.Attributes.x <| String.fromInt ball.x
        , Svg.Attributes.y <| String.fromInt ball.y
        , Svg.Attributes.width <| String.fromInt ball.width
        , Svg.Attributes.height <| String.fromInt ball.height
        ]
        []


viewBallPath : Pong.Ball.ShowBallPath -> List Pong.Ball.Ball -> List (Svg.Svg msg)
viewBallPath showBallPath ballPath =
    case showBallPath of
        Pong.Ball.On ->
            List.indexedMap viewBallPathSegment ballPath

        Pong.Ball.Off ->
            []


viewBallPathSegment : Int -> Pong.Ball.Ball -> Svg.Svg msg
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


viewWinner : Maybe Pong.Paddle.PaddleId -> Html.Html msg
viewWinner maybePaddleId =
    Html.div [ Html.Attributes.class "pt-2" ]
        [ Html.h2 [ Html.Attributes.class "font-bold font-gray-800 pb-1 text-xl" ]
            [ Html.text "Winner!" ]
        , viewWinnerPaddle maybePaddleId
        ]


viewWinnerPaddle : Maybe Pong.Paddle.PaddleId -> Html.Html msg
viewWinnerPaddle maybePaddleId =
    case maybePaddleId of
        Just paddleId ->
            Html.div []
                [ Html.p []
                    [ Html.text <| "\u{1F947} " ++ Pong.Paddle.paddleIdToString paddleId ++ " paddle wins!" ]
                , Html.p []
                    [ Html.text "🆕 Press the SPACEBAR key to reset the game." ]
                ]

        Nothing ->
            Html.span [] []


viewInstructions : Html.Html msg
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


viewOptions : Pong.Ball.ShowBallPath -> Pong.Fps.ShowFps -> Pong.Game.WinningScore -> Html.Html Msg
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


viewShowBallPathOptions : Pong.Ball.ShowBallPath -> Html.Html Msg
viewShowBallPathOptions showBallPath_ =
    Html.fieldset [ Html.Attributes.class "inline" ]
        [ Html.span [ Html.Attributes.class "mr-3" ]
            [ Html.text "Show ball path history:" ]
        , viewRadioButton Pong.Ball.Off showBallPath_ Pong.Ball.showBallPathToString PlayerClickedShowBallPathRadioButton
        , viewRadioButton Pong.Ball.On showBallPath_ Pong.Ball.showBallPathToString PlayerClickedShowBallPathRadioButton
        ]


viewShowFpsOptions : Pong.Fps.ShowFps -> Html.Html Msg
viewShowFpsOptions showFps_ =
    Html.fieldset [ Html.Attributes.class "inline" ]
        [ Html.span [ Html.Attributes.class "mr-3" ]
            [ Html.text "Show FPS meter:" ]
        , viewRadioButton Pong.Fps.Off showFps_ Pong.Fps.showFpsToString PlayerClickedShowFpsRadioButton
        , viewRadioButton Pong.Fps.On showFps_ Pong.Fps.showFpsToString PlayerClickedShowFpsRadioButton
        ]


viewWinningScoreOptions : Pong.Game.WinningScore -> Html.Html Msg
viewWinningScoreOptions winningScore =
    Html.fieldset [ Html.Attributes.class "inline" ]
        [ Html.span [ Html.Attributes.class "mr-3" ]
            [ Html.text "Set winning score:" ]
        , viewRadioButton Pong.Game.Eleven winningScore Pong.Game.winningScoreToString PlayerClickedWinningScoreRadioButton
        , viewRadioButton Pong.Game.Fifteen winningScore Pong.Game.winningScoreToString PlayerClickedWinningScoreRadioButton
        ]



-- VIEW HELPERS


viewRadioButton : a -> a -> (a -> String) -> (a -> Msg) -> Html.Html Msg
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
