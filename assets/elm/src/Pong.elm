module Pong exposing
    ( Model
    , Msg(..)
    , init
    , subscriptions
    , update
    , view
    )

-- IMPORTS

import Browser exposing (Document)
import Browser.Events
import Html exposing (Html)
import Html.Attributes
import Json.Decode
import Json.Encode
import Pong.Ball exposing (Ball, BallPath, ShowBallPath)
import Pong.Game exposing (State, Winner, WinningScore)
import Pong.Paddle exposing (Direction, Paddle)
import Pong.Window exposing (Window, WindowEdge)
import Random exposing (Generator)
import Set
import Svg exposing (Svg)
import Svg.Attributes
import Util.Fps exposing (ShowFps, Time)
import Util.Keyboard exposing (Controls)
import Util.Ports
import Util.View



-- MODEL


type alias Model =
    { ball : Ball
    , ballPath : BallPath
    , deltaTimes : List Time
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
    , deltaTimes = Util.Fps.initialDeltaTimes
    , gameState = Pong.Game.initialState
    , leftPaddle = Pong.Paddle.initialLeftPaddle
    , playerKeyPress = Util.Keyboard.initialKeys
    , rightPaddle = Pong.Paddle.initialRightPaddle
    , showBallPath = Pong.Ball.initialShowBallPath
    , showFps = Util.Fps.initialShowFps
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
    = BrowserAdvancedAnimationFrame Time
    | CollisionGeneratedRandomBallYPositionAndYVelocity ( Float, Float )
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
                windowEdgeHitByBall =
                    Pong.Window.getWindowEdgeHitByBall model.ball Pong.Window.globalWindow

                paddleHitByBall =
                    Pong.Paddle.getPaddleHitByBall model.ball model.leftPaddle model.rightPaddle

                winner =
                    Pong.Game.getWinner model.leftPaddle model.rightPaddle model.winningScore

                leftPaddleDirection =
                    Pong.Paddle.playerKeyPressToDirection model.playerKeyPress
            in
            model
                |> updateBall model.ball paddleHitByBall windowEdgeHitByBall deltaTime
                |> updateBallPath model.ball model.ballPath windowEdgeHitByBall
                |> updateDeltaTimes model.showFps deltaTime
                |> updatePaddle model.leftPaddle leftPaddleDirection model.ball Pong.Window.globalWindow deltaTime
                |> updatePaddle model.rightPaddle Nothing model.ball Pong.Window.globalWindow deltaTime
                |> updatePaddleScores windowEdgeHitByBall
                |> updateWinner winner
                |> updateGameState model.gameState winner
                |> addCommand paddleHitByBall windowEdgeHitByBall

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
                            ( updateGameState Pong.Game.StartingScreen Nothing initialModel, Cmd.none )

                _ ->
                    ( updateKeyPress key model, Cmd.none )

        PlayerReleasedKey _ ->
            ( { model | playerKeyPress = Set.empty }, Cmd.none )



-- UPDATES


updateBall : Ball -> Maybe Paddle -> Maybe WindowEdge -> Time -> Model -> Model
updateBall ball maybePaddle maybeWindowEdge deltaTime model =
    { model | ball = updateBallWithCollisions ball maybePaddle maybeWindowEdge deltaTime }


updateBallWithCollisions : Ball -> Maybe Paddle -> Maybe WindowEdge -> Time -> Ball
updateBallWithCollisions ball maybePaddle maybeWindowEdge deltaTime =
    let
        ballSpeedChangeAfterCollision =
            50

        ballAngleChangeMultiplier =
            6

        limitBallSpeedChange =
            clamp -650 650
    in
    case ( maybePaddle, maybeWindowEdge ) of
        ( Just paddle, Nothing ) ->
            case paddle.id of
                Pong.Paddle.Left ->
                    { ball
                        | x = ball.x + ball.width
                        , vx = limitBallSpeedChange <| negate <| ball.vx - ballSpeedChangeAfterCollision
                        , vy = ballAngleChangeMultiplier * Pong.Paddle.getPaddleHitByBallDistanceFromCenter ball paddle
                    }

                Pong.Paddle.Right ->
                    { ball
                        | x = ball.x - ball.width
                        , vx = limitBallSpeedChange <| negate <| ball.vx + ballSpeedChangeAfterCollision
                        , vy = ballAngleChangeMultiplier * Pong.Paddle.getPaddleHitByBallDistanceFromCenter ball paddle
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
                        | x = Pong.Ball.initialBall.x + 100
                        , vx = negate Pong.Ball.initialBall.vx
                        , vy = Pong.Ball.initialBall.vy
                    }

                Pong.Window.Right ->
                    { ball
                        | x = Pong.Ball.initialBall.x - 100
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
                        , vx = limitBallSpeedChange <| negate <| ball.vx - ballSpeedChangeAfterCollision
                    }

                Pong.Paddle.Right ->
                    { ball
                        | x = ball.x - ball.width
                        , vx = limitBallSpeedChange <| negate <| ball.vx + ballSpeedChangeAfterCollision
                    }

        ( Nothing, Nothing ) ->
            { ball
                | x = ball.x + ball.vx * deltaTime
                , y = ball.y + ball.vy * deltaTime
            }


updateBallWithRandomness : Float -> Float -> Ball -> Ball
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


updateDeltaTimes : ShowFps -> Time -> Model -> Model
updateDeltaTimes showFps deltaTime model =
    case showFps of
        Util.Fps.Off ->
            model

        Util.Fps.On ->
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
    if Set.member key Util.Keyboard.validKeys then
        { model | playerKeyPress = Set.insert key model.playerKeyPress }

    else
        model


updatePaddle : Paddle -> Maybe Direction -> Ball -> Window -> Time -> Model -> Model
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


randomBallYPositionGenerator : Generator Float
randomBallYPositionGenerator =
    Random.float
        (Pong.Window.globalWindow.y + 100.0)
        (Pong.Window.globalWindow.height - 100.0)


randomBallYVelocityGenerator : Generator Float
randomBallYVelocityGenerator =
    Random.float (negate Pong.Ball.initialBall.vy) Pong.Ball.initialBall.vy


randomBallPositionAndVelocity : Generator ( Float, Float )
randomBallPositionAndVelocity =
    Random.pair randomBallYPositionGenerator randomBallYVelocityGenerator


generateRandomBallPosition : Cmd Msg
generateRandomBallPosition =
    Random.generate CollisionGeneratedRandomBallYPositionAndYVelocity randomBallPositionAndVelocity


playSoundCommand : String -> Cmd Msg
playSoundCommand soundFile =
    Util.Ports.playSound <| Json.Encode.string soundFile



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


handleAnimationFrames : Time -> Msg
handleAnimationFrames milliseconds =
    BrowserAdvancedAnimationFrame <| milliseconds / 1000


keyDownSubscription : Sub Msg
keyDownSubscription =
    Browser.Events.onKeyDown <| Json.Decode.map PlayerPressedKeyDown <| Util.Keyboard.keyDecoder


keyUpSubscription : Sub Msg
keyUpSubscription =
    Browser.Events.onKeyUp <| Json.Decode.map PlayerReleasedKey <| Util.Keyboard.keyDecoder



-- VIEW


view : (Msg -> msg) -> Model -> Document msg
view msg model =
    { title = "\u{1F3D3} Pong"
    , body = List.map (Html.map msg) [ viewMain model, Util.View.footer ]
    }


viewMain : Model -> Html Msg
viewMain model =
    Html.main_ [ Html.Attributes.class "bg-yellow-200 h-full p-8" ]
        [ viewHeader
        , viewGame model
        , viewInformation model
        ]


viewHeader : Html msg
viewHeader =
    Html.header [ Html.Attributes.class "flex justify-center" ]
        [ logo ]


logo : Svg msg
logo =
    Svg.svg
        [ Svg.Attributes.version "1.0"
        , Svg.Attributes.width "400"
        , Svg.Attributes.height "75"
        , Svg.Attributes.viewBox "0 0 400 75"
        ]
        [ Svg.g
            [ Svg.Attributes.transform "translate(75,75) scale(0.03,-0.03)"
            , Svg.Attributes.fill "black"
            ]
            [ Svg.path [ Svg.Attributes.d "M2785 2319 c-356 -51 -684 -291 -840 -613 -206 -425 -123 -920 210 -1251 239 -238 582 -357 937 -325 298 26 520 124 715 313 184 179 285 388 313 652 23 222 -18 443 -118 639 -60 116 -114 187 -221 288 -172 161 -400 265 -651 298 -90 11 -262 11 -345 -1z m390 -439 c240 -75 419 -250 501 -490 27 -78 27 -282 0 -360 -38 -112 -88 -192 -171 -275 -84 -84 -154 -131 -255 -172 -399 -161 -844 61 -957 477 -28 103 -23 264 10 365 78 236 274 414 520 470 89 21 262 13 352 -15z" ] []
            , Svg.path [ Svg.Attributes.d "M5108 2320 c-288 -46 -512 -238 -578 -496 -19 -74 -20 -114 -20 -891 l0 -813 220 0 220 0 2 803 3 802 22 41 c50 94 146 153 262 162 137 9 247 -38 306 -133 l30 -48 5 -811 5 -811 220 0 220 0 0 810 0 810 -23 75 c-71 229 -266 404 -534 480 -84 24 -269 34 -360 20z" ] []
            , Svg.path [ Svg.Attributes.d "M7370 2304 c-153 -26 -235 -52 -355 -112 -129 -64 -218 -125 -313 -216 -238 -228 -344 -497 -329 -836 15 -335 180 -647 450 -851 141 -106 318 -182 489 -209 107 -17 379 -8 478 17 l75 18 3 553 2 552 -330 0 -330 0 0 -200 0 -200 100 0 100 0 0 -166 0 -167 -51 7 c-189 25 -394 209 -486 436 -74 183 -65 348 26 535 81 165 226 292 401 351 161 54 340 54 487 -1 36 -14 69 -25 74 -25 5 0 9 104 9 230 l0 230 -27 11 c-73 28 -159 40 -298 44 -82 2 -161 2 -175 -1z" ] []
            , Svg.path [ Svg.Attributes.d "M122 1203 l3 -1088 215 0 215 0 3 406 2 406 173 6 c195 6 251 17 372 76 176 87 310 249 360 437 25 92 30 235 12 332 -36 188 -156 346 -327 429 -154 75 -155 76 -618 80 l-412 5 2 -1089z m783 686 c55 -15 137 -100 158 -164 46 -134 -14 -285 -143 -362 l-54 -33 -148 0 -148 0 0 278 c0 153 3 282 7 285 11 11 285 8 328 -4z" ] []
            ]
        ]


viewGame : Model -> Html Msg
viewGame model =
    Html.section [ Html.Attributes.class "flex justify-center my-4" ]
        [ Html.div [ Html.Attributes.class "flex-shrink-0" ]
            [ viewSvg Pong.Window.globalWindow model ]
        ]


viewSvg : Window -> Model -> Svg msg
viewSvg window model =
    let
        leftPaddleScoreOffset =
            -200.0

        rightPaddleScoreOffset =
            150.0

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
        ]
        [ Pong.Window.viewGameWindow window
        , Pong.Window.viewNet window
        , Pong.Paddle.viewPaddleScore model.leftPaddle.score window leftPaddleScoreOffset
        , Pong.Paddle.viewPaddleScore model.rightPaddle.score window rightPaddleScoreOffset
        , Pong.Paddle.viewPaddle model.leftPaddle
        , Pong.Paddle.viewPaddle model.rightPaddle
        , Pong.Ball.viewBall model.ball
        , Pong.Ball.viewBallPath model.showBallPath model.ballPath |> Svg.g []
        , Util.Fps.viewFps model.showFps model.deltaTimes
        ]



-- VIEW INFO


viewInformation : Model -> Html Msg
viewInformation model =
    Html.section []
        [ viewWinner model.gameState model.winner
        , viewInstructions
        , viewOptions model.showBallPath model.showFps model.winningScore
        ]



-- WINNER


viewWinner : State -> Maybe Paddle -> Html msg
viewWinner gameState maybePaddle =
    case gameState of
        Pong.Game.StartingScreen ->
            Html.span [] []

        Pong.Game.PlayingScreen ->
            Html.span [] []

        Pong.Game.EndingScreen ->
            Html.div [ Html.Attributes.class "pt-4 text-center" ]
                [ Html.h2 [ Html.Attributes.class "font-extrabold font-gray-800 pb-1 text-xl" ]
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



-- INSTRUCTIONS


viewInstructions : Html msg
viewInstructions =
    Html.div [ Html.Attributes.class "pt-4" ]
        [ Html.h2 [ Html.Attributes.class "font-extrabold font-gray-800 pb-1 text-center text-xl" ]
            [ Html.text "Instructions" ]
        , Html.div [ Html.Attributes.class "flex justify-center" ]
            [ Html.ul [ Html.Attributes.class "leading-relaxed list-disc list-inside mx-3" ]
                [ Html.li [] [ Html.text "\u{1F3D3} Press the SPACEBAR key to serve the ball." ]
                , Html.li [] [ Html.text "⌨️ Use the arrow keys to move the left paddle." ]
                , Html.li [] [ Html.text "🏆 Avoid missing ball for high score." ]
                ]
            ]
        ]



-- OPTIONS


viewOptions : ShowBallPath -> ShowFps -> WinningScore -> Html Msg
viewOptions showBallPath_ showFps winningScore =
    Html.div [ Html.Attributes.class "pt-4" ]
        [ Html.h2 [ Html.Attributes.class "font-extrabold font-gray-800 pb-1 text-center text-xl" ]
            [ Html.text "Options" ]
        , Html.form [ Html.Attributes.class "flex justify-center" ]
            [ Html.ul [ Html.Attributes.class "leading-relaxed list-disc list-inside mx-3" ]
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
        , Util.View.radioButton Pong.Ball.Off showBallPath_ Pong.Ball.showBallPathToString PlayerClickedShowBallPathRadioButton
        , Util.View.radioButton Pong.Ball.On showBallPath_ Pong.Ball.showBallPathToString PlayerClickedShowBallPathRadioButton
        ]


viewShowFpsOptions : ShowFps -> Html Msg
viewShowFpsOptions showFps_ =
    Html.fieldset [ Html.Attributes.class "inline" ]
        [ Html.span [ Html.Attributes.class "mr-3" ]
            [ Html.text "Show FPS meter:" ]
        , Util.View.radioButton Util.Fps.Off showFps_ Util.Fps.showFpsToString PlayerClickedShowFpsRadioButton
        , Util.View.radioButton Util.Fps.On showFps_ Util.Fps.showFpsToString PlayerClickedShowFpsRadioButton
        ]


viewWinningScoreOptions : WinningScore -> Html Msg
viewWinningScoreOptions winningScore =
    Html.fieldset [ Html.Attributes.class "inline" ]
        [ Html.span [ Html.Attributes.class "mr-3" ]
            [ Html.text "Set winning score:" ]
        , Util.View.radioButton Pong.Game.Eleven winningScore Pong.Game.winningScoreToString PlayerClickedWinningScoreRadioButton
        , Util.View.radioButton Pong.Game.Fifteen winningScore Pong.Game.winningScoreToString PlayerClickedWinningScoreRadioButton
        ]
