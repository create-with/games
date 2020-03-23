port module Pong exposing
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
import Set
import Svg
import Svg.Attributes



-- MODEL


type alias Ball =
    { color : String
    , x : Int
    , y : Int
    , vx : Float
    , vy : Float
    , width : Int
    , height : Int
    }


initialBall : Ball
initialBall =
    { color = "white"
    , x = 400
    , y = 300
    , vx = 0.4
    , vy = 0.4
    , width = 10
    , height = 10
    }


type GameState
    = StartingScreen
    | PlayingScreen
    | EndingScreen


type alias Paddle =
    { color : String
    , id : PaddleId
    , x : Int
    , y : Int
    , width : Int
    , height : Int
    }


initialLeftPaddle : Paddle
initialLeftPaddle =
    { color = "lightblue"
    , id = Left
    , x = 48
    , y = 200
    , width = 10
    , height = 60
    }


initialRightPaddle : Paddle
initialRightPaddle =
    { color = "lightpink"
    , id = Right
    , x = 740
    , y = 300
    , width = 10
    , height = 60
    }


type PaddleId
    = Left
    | Right


type ShowBallPath
    = On
    | Off


type alias Window =
    { backgroundColor : String
    , x : Int
    , y : Int
    , width : Int
    , height : Int
    }


globalWindow : Window
globalWindow =
    { backgroundColor = "black"
    , x = 0
    , y = 0
    , width = 800
    , height = 600
    }


type WindowEdge
    = Top
    | Bottom


type WinningScore
    = Eleven
    | Fifteen


type alias Model =
    { ball : Ball
    , ballPath : List Ball
    , gameState : GameState
    , leftPaddle : Paddle
    , leftPaddleScore : Int
    , playerKeyPress : Set.Set String
    , rightPaddle : Paddle
    , rightPaddleScore : Int
    , showBallPath : ShowBallPath
    , winner : Maybe PaddleId
    , winningScore : WinningScore
    }


initialModel : Model
initialModel =
    { ball = initialBall
    , ballPath = []
    , gameState = StartingScreen
    , leftPaddle = initialLeftPaddle
    , leftPaddleScore = 0
    , playerKeyPress = Set.empty
    , rightPaddle = initialRightPaddle
    , rightPaddleScore = 0
    , showBallPath = Off
    , winner = Nothing
    , winningScore = Eleven
    }


initialCommand : Cmd Msg
initialCommand =
    Cmd.none


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, initialCommand )



-- UPDATE


type Msg
    = BrowserAdvancedAnimationFrame Float
    | PlayerClickedShowBallPathRadioButton ShowBallPath
    | PlayerClickedWinningScoreRadioButton WinningScore
    | PlayerPressedKeyDown String
    | PlayerReleasedKey String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BrowserAdvancedAnimationFrame time ->
            case model.gameState of
                StartingScreen ->
                    if playerPressedSpacebarKey model.playerKeyPress then
                        model
                            |> updateGameState PlayingScreen
                            |> noCommand

                    else
                        model |> noCommand

                PlayingScreen ->
                    if leftPaddleHasWinningScore model.leftPaddleScore model.winningScore then
                        model
                            |> updateGameState EndingScreen
                            |> updateWinner (Just model.leftPaddle.id)
                            |> noCommand

                    else if rightPaddleHasWinningScore model.rightPaddleScore model.winningScore then
                        model
                            |> updateGameState EndingScreen
                            |> updateWinner (Just model.rightPaddle.id)
                            |> noCommand

                    else if ballHitLeftPaddle model.ball model.leftPaddle then
                        model
                            |> updateBall model.ball (Just model.leftPaddle) Nothing time
                            |> playSoundCommand "beep.wav"

                    else if ballHitRightPaddle model.ball model.rightPaddle then
                        model
                            |> updateBall model.ball (Just model.rightPaddle) Nothing time
                            |> playSoundCommand "beep.wav"

                    else if ballHitRightEdge model.ball globalWindow then
                        model
                            |> updateBall initialBall Nothing Nothing time
                            |> updateBallPath []
                            |> updateLeftPaddleScore (model.leftPaddleScore + 1)
                            |> noCommand

                    else if ballHitLeftEdge model.ball globalWindow then
                        model
                            |> updateBall initialBall Nothing Nothing time
                            |> updateBallPath []
                            |> updateRightPaddleScore (model.rightPaddleScore + 1)
                            |> noCommand

                    else if ballHitTopEdge model.ball globalWindow then
                        model
                            |> updateBall model.ball Nothing (Just Top) time
                            |> playSoundCommand "boop"

                    else if ballHitBottomEdge model.ball globalWindow then
                        model
                            |> updateBall model.ball Nothing (Just Bottom) time
                            |> playSoundCommand "boop"

                    else if playerPressedArrowUpKey model.playerKeyPress then
                        model
                            |> updateBall model.ball Nothing Nothing time
                            |> updateBallPath (List.take 99 <| model.ball :: model.ballPath)
                            |> updateLeftPaddle (movePaddleUp model.leftPaddle)
                            |> updateRightPaddle model.rightPaddle model.ball
                            |> noCommand

                    else if playerPressedArrowDownKey model.playerKeyPress then
                        model
                            |> updateBall model.ball Nothing Nothing time
                            |> updateBallPath (List.take 99 <| model.ball :: model.ballPath)
                            |> updateLeftPaddle (movePaddleDown model.leftPaddle)
                            |> updateRightPaddle model.rightPaddle model.ball
                            |> noCommand

                    else
                        model
                            |> updateBall model.ball Nothing Nothing time
                            |> updateBallPath (List.take 99 <| model.ball :: model.ballPath)
                            |> updateRightPaddle model.rightPaddle model.ball
                            |> noCommand

                EndingScreen ->
                    model |> noCommand

        PlayerClickedShowBallPathRadioButton showBallPathValue ->
            model
                |> updateShowBallPath showBallPathValue
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


ballHitTopEdge : Ball -> Window -> Bool
ballHitTopEdge ball window =
    (ball.y - ball.height) <= window.x


ballHitBottomEdge : Ball -> Window -> Bool
ballHitBottomEdge ball window =
    (ball.y + ball.height) >= window.height


ballHitLeftEdge : Ball -> Window -> Bool
ballHitLeftEdge ball window =
    (ball.x - ball.width) <= window.x


ballHitRightEdge : Ball -> Window -> Bool
ballHitRightEdge ball window =
    (ball.x + ball.width) >= window.width


ballHitLeftPaddle : Ball -> Paddle -> Bool
ballHitLeftPaddle ball paddle =
    (paddle.y < ball.y && ball.y < paddle.y + paddle.height)
        && (paddle.x < ball.x && ball.x < paddle.x + paddle.width)


ballHitRightPaddle : Ball -> Paddle -> Bool
ballHitRightPaddle ball paddle =
    (paddle.y < ball.y && ball.y < paddle.y + paddle.height)
        && (paddle.x < ball.x + ball.width && ball.x < paddle.x + paddle.width)


ballHitRightPaddleLocations : Ball -> Paddle -> ( Int, Int )
ballHitRightPaddleLocations ball paddle =
    if ballHitRightPaddle ball paddle then
        ( ball.y, paddle.y )

    else
        ( 0, 0 )


leftPaddleHasWinningScore : Int -> WinningScore -> Bool
leftPaddleHasWinningScore leftPaddleScore winningScore =
    leftPaddleScore == winningScoreToInt winningScore


rightPaddleHasWinningScore : Int -> WinningScore -> Bool
rightPaddleHasWinningScore rightPaddleScore winningScore =
    rightPaddleScore == winningScoreToInt winningScore



-- UPDATES


updateBall : Ball -> Maybe Paddle -> Maybe WindowEdge -> Float -> Model -> Model
updateBall newBall maybePaddle maybeEdge time model =
    let
        applyUpdates ball =
            case ( maybePaddle, maybeEdge ) of
                -- ball hit paddle, ball did not hit edge
                ( Just paddle, Nothing ) ->
                    case paddle.id of
                        Left ->
                            { ball
                                | x = ball.x + ball.width
                                , vx = negate <| (ball.vx - 0.033)
                            }

                        Right ->
                            { ball
                                | x = ball.x - ball.width
                                , vx = negate <| (ball.vx + 0.033)
                            }

                -- ball did not hit paddle, ball hit edge
                ( Nothing, Just edge ) ->
                    case edge of
                        Top ->
                            { ball
                                | y = ball.y + ball.height
                                , vy = negate ball.vy
                            }

                        Bottom ->
                            { ball
                                | y = ball.y - ball.height
                                , vy = negate ball.vy
                            }

                -- ball hit paddle, ball hit edge (hadn't thought about this case)
                ( Just _, Just _ ) ->
                    { ball
                        | x = round <| toFloat ball.x + ball.vx * time
                        , y = round <| toFloat ball.y + ball.vy * time
                    }

                -- ball did not hit paddle, ball did not hit edge
                ( Nothing, Nothing ) ->
                    { ball
                        | x = round <| toFloat ball.x + ball.vx * time
                        , y = round <| toFloat ball.y + ball.vy * time
                    }
    in
    { model | ball = applyUpdates newBall }


updateBallPath : List Ball -> Model -> Model
updateBallPath newBallPath model =
    { model | ballPath = newBallPath }


updateGameState : GameState -> Model -> Model
updateGameState newGameState model =
    { model | gameState = newGameState }


updateLeftPaddleScore : Int -> Model -> Model
updateLeftPaddleScore newLeftPaddleScore model =
    { model | leftPaddleScore = newLeftPaddleScore }


updateLeftPaddle : Paddle -> Model -> Model
updateLeftPaddle newLeftPaddle model =
    { model | leftPaddle = newLeftPaddle }


updateRightPaddle : Paddle -> Ball -> Model -> Model
updateRightPaddle newRightPaddle ball model =
    let
        updatePaddle paddle =
            if ball.y > paddle.y then
                { paddle | y = keepPaddleInWindow paddle (paddle.y + 5) globalWindow }

            else if ball.y < paddle.y then
                { paddle | y = keepPaddleInWindow paddle (paddle.y - 5) globalWindow }

            else
                paddle
    in
    { model | rightPaddle = updatePaddle newRightPaddle }


updateRightPaddleScore : Int -> Model -> Model
updateRightPaddleScore newRightPaddleScore model =
    { model | rightPaddleScore = newRightPaddleScore }


updateShowBallPath : ShowBallPath -> Model -> Model
updateShowBallPath newShowBallPath model =
    let
        _ =
            Debug.log "show Ball path" newShowBallPath
    in
    { model | showBallPath = newShowBallPath }


updateWinner : Maybe PaddleId -> Model -> Model
updateWinner newWinner model =
    { model | winner = newWinner }


updateWinningScore : WinningScore -> Model -> Model
updateWinningScore newWinningScore model =
    { model | winningScore = newWinningScore }



-- COMMANDS


noCommand : Model -> ( Model, Cmd Msg )
noCommand model =
    ( model, Cmd.none )


playSoundCommand : String -> Model -> ( Model, Cmd Msg )
playSoundCommand soundFile model =
    ( model, playSound <| Json.Encode.string soundFile )



-- UPDATE HELPERS


keepPaddleInWindow : Paddle -> Int -> Window -> Int
keepPaddleInWindow paddle paddleYPosition window =
    let
        topEdge =
            window.y

        bottomEdge =
            window.height - paddle.height
    in
    Basics.clamp topEdge bottomEdge paddleYPosition


movePaddleDown : Paddle -> Paddle
movePaddleDown paddle =
    { paddle | y = keepPaddleInWindow paddle (paddle.y + 10) globalWindow }


movePaddleUp : Paddle -> Paddle
movePaddleUp paddle =
    { paddle | y = keepPaddleInWindow paddle (paddle.y - 10) globalWindow }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta BrowserAdvancedAnimationFrame
        , Browser.Events.onKeyDown <| Json.Decode.map PlayerPressedKeyDown <| keyDecoder
        , Browser.Events.onKeyUp <| Json.Decode.map PlayerReleasedKey <| keyDecoder
        ]



-- KEYBOARD INPUT HANDLING


keyDecoder : Json.Decode.Decoder String
keyDecoder =
    Json.Decode.field "key" Json.Decode.string


clearKeyPresses : Model -> Model
clearKeyPresses model =
    { model | playerKeyPress = Set.empty }


playerPressedKey : Set.Set String -> Bool
playerPressedKey playerKeyPress =
    (Set.isEmpty >> not) playerKeyPress


playerPressedSpacebarKey : Set.Set String -> Bool
playerPressedSpacebarKey playerKeyPress =
    Set.member " " playerKeyPress


playerPressedArrowUpKey : Set.Set String -> Bool
playerPressedArrowUpKey playerKeyPress =
    Set.member "ArrowUp" playerKeyPress


playerPressedArrowDownKey : Set.Set String -> Bool
playerPressedArrowDownKey playerKeyPress =
    Set.member "ArrowDown" playerKeyPress


updateKeyPress : String -> Model -> Model
updateKeyPress key model =
    if Set.member key validKeys then
        { model | playerKeyPress = Set.insert key model.playerKeyPress }

    else
        model


validKeys : Set.Set String
validKeys =
    Set.empty
        |> Set.insert "ArrowUp"
        |> Set.insert "ArrowDown"
        |> Set.insert " "



-- VIEW


view : Model -> Html.Html Msg
view model =
    Html.main_ [ Html.Attributes.class "px-6" ]
        [ Html.header []
            [ Html.h1 [ Html.Attributes.class "font-black text-black text-5xl" ] [ Html.text "\u{1F3D3} Pong" ]
            ]
        , Html.section []
            [ case model.gameState of
                StartingScreen ->
                    Html.div []
                        [ viewSvg globalWindow model
                        , viewInstructions
                        , viewOptions model.showBallPath model.winningScore
                        ]

                PlayingScreen ->
                    Html.div []
                        [ viewSvg globalWindow model
                        , viewInstructions
                        , viewOptions model.showBallPath model.winningScore
                        ]

                EndingScreen ->
                    Html.div []
                        [ viewSvg globalWindow model
                        , viewWinner model.winner
                        , viewInstructions
                        , viewOptions model.showBallPath model.winningScore
                        ]
            ]
        ]


viewSvg : Window -> Model -> Svg.Svg msg
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
         , viewPaddleScore model.leftPaddleScore window -200
         , viewPaddleScore model.rightPaddleScore window 150
         , viewPaddle model.leftPaddle
         , viewPaddle model.rightPaddle
         , viewBall model.ball
         ]
            ++ (case model.showBallPath of
                    On ->
                        viewBallPath model.ballPath

                    Off ->
                        [ Html.span [] [] ]
               )
        )


viewGameWindow : Window -> Svg.Svg msg
viewGameWindow window =
    Svg.rect
        [ Svg.Attributes.fill window.backgroundColor
        , Svg.Attributes.x <| String.fromInt window.x
        , Svg.Attributes.y <| String.fromInt window.y
        , Svg.Attributes.width <| String.fromInt window.width
        , Svg.Attributes.height <| String.fromInt window.height
        ]
        []


viewNet : Window -> Svg.Svg msg
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


viewPaddleScore : Int -> Window -> Int -> Svg.Svg msg
viewPaddleScore score window positionOffset =
    Svg.text_
        [ Svg.Attributes.fill "white"
        , Svg.Attributes.fontFamily "Courier New"
        , Svg.Attributes.fontSize "80"
        , Svg.Attributes.fontWeight "bold"
        , Svg.Attributes.x <| String.fromInt <| (window.width // 2) + positionOffset
        , Svg.Attributes.y "100"
        ]
        [ Svg.text <| String.fromInt score ]


viewPaddle : Paddle -> Svg.Svg msg
viewPaddle paddle =
    Svg.rect
        [ Svg.Attributes.fill paddle.color
        , Svg.Attributes.x <| String.fromInt <| paddle.x
        , Svg.Attributes.y <| String.fromInt <| paddle.y
        , Svg.Attributes.width <| String.fromInt <| paddle.width
        , Svg.Attributes.height <| String.fromInt <| paddle.height
        ]
        []


viewBall : Ball -> Svg.Svg msg
viewBall ball =
    Svg.rect
        [ Svg.Attributes.fill ball.color
        , Svg.Attributes.x <| String.fromInt <| ball.x
        , Svg.Attributes.y <| String.fromInt <| ball.y
        , Svg.Attributes.width <| String.fromInt ball.width
        , Svg.Attributes.height <| String.fromInt ball.height
        ]
        []


viewBallPath : List Ball -> List (Svg.Svg msg)
viewBallPath ballPath =
    ballPath
        |> List.indexedMap
            (\index ball ->
                Svg.rect
                    [ Svg.Attributes.fillOpacity <| String.fromFloat <| 0.01 * toFloat (80 - index)
                    , Svg.Attributes.fill "darkorange"
                    , Svg.Attributes.x <| String.fromInt <| ball.x
                    , Svg.Attributes.y <| String.fromInt <| ball.y
                    , Svg.Attributes.width <| String.fromInt ball.width
                    , Svg.Attributes.height <| String.fromInt ball.height
                    ]
                    []
            )


viewWinner : Maybe PaddleId -> Html.Html msg
viewWinner maybePaddleId =
    Html.div [ Html.Attributes.class "pt-2" ]
        [ Html.h2 [ Html.Attributes.class "font-bold font-gray-800 pb-1 text-xl" ]
            [ Html.text "Winner!" ]
        , case maybePaddleId of
            Just paddleId ->
                Html.p [] [ Html.text <| "\u{1F947} " ++ paddleIdToString paddleId ++ " paddle wins!" ]

            Nothing ->
                Html.span [] []
        ]


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


viewOptions : ShowBallPath -> WinningScore -> Html.Html Msg
viewOptions showBallPath_ winningScore =
    Html.div [ Html.Attributes.class "pt-2" ]
        [ Html.h2 [ Html.Attributes.class "font-bold font-gray-800 pb-1 text-xl" ]
            [ Html.text "Options" ]
        , Html.form []
            [ viewShowBallPathOptions showBallPath_
            , viewWinningScoreOptions winningScore
            ]
        ]


viewShowBallPathOptions : ShowBallPath -> Html.Html Msg
viewShowBallPathOptions showBallPath_ =
    Html.fieldset []
        [ Html.span [ Html.Attributes.class "font-medium italic mr-3" ]
            [ Html.text "Show ball path history:" ]
        , viewRadioButton Off showBallPath_ showBallPathToString PlayerClickedShowBallPathRadioButton
        , viewRadioButton On showBallPath_ showBallPathToString PlayerClickedShowBallPathRadioButton
        ]


viewWinningScoreOptions : WinningScore -> Html.Html Msg
viewWinningScoreOptions winningScore =
    Html.fieldset []
        [ Html.span [ Html.Attributes.class "font-medium italic mr-3" ]
            [ Html.text "Set winning score:" ]
        , viewRadioButton Eleven winningScore winningScoreToString PlayerClickedWinningScoreRadioButton
        , viewRadioButton Fifteen winningScore winningScoreToString PlayerClickedWinningScoreRadioButton
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



-- CONVERSION HELPERS


paddleIdToString : PaddleId -> String
paddleIdToString paddleId =
    case paddleId of
        Left ->
            "Left"

        Right ->
            "Right"


showBallPathToString : ShowBallPath -> String
showBallPathToString showBallPath_ =
    case showBallPath_ of
        On ->
            "On"

        Off ->
            "Off"


winningScoreToInt : WinningScore -> Int
winningScoreToInt winningScore =
    case winningScore of
        Eleven ->
            11

        Fifteen ->
            15


winningScoreToString : WinningScore -> String
winningScoreToString winningScore =
    case winningScore of
        Eleven ->
            "11"

        Fifteen ->
            "15"



-- PORTS


port playSound : Json.Encode.Value -> Cmd msg
