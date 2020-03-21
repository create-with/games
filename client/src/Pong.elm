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
    , vx = 0.33
    , vy = 0.33
    , width = 10
    , height = 10
    }


type GameState
    = Start
    | Playing
    | End


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
    , width = 12
    , height = 64
    }


initialRightPaddle : Paddle
initialRightPaddle =
    { color = "lightpink"
    , id = Right
    , x = 740
    , y = 300
    , width = 12
    , height = 64
    }


type PaddleId
    = Left
    | Right


type alias Window =
    { backgroundColor : String
    , x : Int
    , y : Int
    , width : Int
    , height : Int
    }


initialWindow : Window
initialWindow =
    { backgroundColor = "black"
    , x = 0
    , y = 0
    , width = 800
    , height = 600
    }


type alias Model =
    { ball : Ball
    , ballPath : List Ball
    , ballPathToggle : Bool
    , gameState : GameState
    , leftPaddle : Paddle
    , leftPaddleScore : Int
    , playerKeyPress : Set.Set String
    , rightPaddle : Paddle
    , rightPaddleScore : Int
    , winner : Maybe PaddleId
    }


initialModel : Model
initialModel =
    { ball = initialBall
    , ballPath = []
    , ballPathToggle = False
    , gameState = Start
    , leftPaddle = initialLeftPaddle
    , leftPaddleScore = 0
    , playerKeyPress = Set.empty
    , rightPaddle = initialRightPaddle
    , rightPaddleScore = 0
    , winner = Nothing
    }


initialCommand : Cmd Msg
initialCommand =
    Cmd.none


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, initialCommand )



-- UPDATE


type Msg
    = GameLoop Float
    | PlayerCheckedBallPathToggle Bool
    | PlayerPressedKeyDown String
    | PlayerPressedKeyUp String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GameLoop frame ->
            case model.gameState of
                Start ->
                    if Set.member " " model.playerKeyPress then
                        ( { model | gameState = Playing }, Cmd.none )

                    else
                        ( model, Cmd.none )

                Playing ->
                    if model.leftPaddleScore == 11 then
                        ( { model
                            | gameState = End
                            , winner = Just model.leftPaddle.id
                          }
                        , Cmd.none
                        )

                    else if model.rightPaddleScore == 11 then
                        ( { model
                            | gameState = End
                            , winner = Just model.rightPaddle.id
                          }
                        , Cmd.none
                        )

                    else if ballHitLeftPaddle model.ball model.leftPaddle then
                        let
                            updatedBall ball =
                                { ball
                                    | x = ball.x + ball.width
                                    , vx = negate <| (ball.vx - 0.033)
                                }
                        in
                        ( { model | ball = updatedBall model.ball }, playSound <| Json.Encode.string "beep.wav" )

                    else if ballHitRightPaddle model.ball model.rightPaddle then
                        let
                            ( ballHitSpot, paddleHitSpot ) =
                                ballHitRightPaddleLocations model.ball model.rightPaddle

                            updatedBall ball =
                                { ball
                                    | x = ball.x - ball.width
                                    , vx = negate <| (ball.vx + 0.033)

                                    -- , vx = negate <| ball.vx * Basics.cos (Basics.degrees (75.0 * (toFloat model.rightPaddle.y + (toFloat model.rightPaddle.height / 2) - toFloat ballHitSpot) / (toFloat model.rightPaddle.height / 2)))
                                }
                        in
                        ( { model | ball = updatedBall model.ball }, playSound <| Json.Encode.string "beep.wav" )

                    else if ballHitRightEdge model.ball initialWindow then
                        ( { model
                            | ball = initialModel.ball
                            , ballPath = []
                            , leftPaddleScore = model.leftPaddleScore + 1
                          }
                        , Cmd.none
                        )

                    else if ballHitLeftEdge model.ball then
                        ( { model
                            | ball = initialModel.ball
                            , ballPath = []
                            , rightPaddleScore = model.rightPaddleScore + 1
                          }
                        , Cmd.none
                        )

                    else if ballHitTopEdge model.ball then
                        let
                            updatedBall ball =
                                { ball
                                    | y = ball.y + ball.height
                                    , vy = negate ball.vy
                                }
                        in
                        ( { model | ball = updatedBall model.ball }, playSound (Json.Encode.string "boop.wav") )

                    else if ballHitBottomEdge model.ball initialWindow then
                        let
                            updatedBall ball =
                                { ball
                                    | y = ball.y - ball.height
                                    , vy = negate ball.vy
                                }
                        in
                        ( { model | ball = updatedBall model.ball }, playSound (Json.Encode.string "boop.wav") )

                    else if playerPressedKey model.playerKeyPress then
                        if Set.member "ArrowUp" model.playerKeyPress then
                            ( { model
                                | ball = updateBallPosition frame model.ball
                                , ballPath = List.take 99 <| model.ball :: model.ballPath
                                , leftPaddle = movePaddleUp model.leftPaddle
                                , rightPaddle = updateAIPaddlePosition frame model.ball model.rightPaddle
                              }
                            , Cmd.none
                            )

                        else if Set.member "ArrowDown" model.playerKeyPress then
                            ( { model
                                | ball = updateBallPosition frame model.ball
                                , ballPath = List.take 99 <| model.ball :: model.ballPath
                                , leftPaddle = movePaddleDown model.leftPaddle
                                , rightPaddle = updateAIPaddlePosition frame model.ball model.rightPaddle
                              }
                            , Cmd.none
                            )

                        else if Set.member " " model.playerKeyPress then
                            ( { model
                                | ball = updateBallPosition frame model.ball
                                , ballPath = List.take 99 <| model.ball :: model.ballPath
                                , rightPaddle = updateAIPaddlePosition frame model.ball model.rightPaddle
                              }
                            , Cmd.none
                            )

                        else
                            ( model, Cmd.none )

                    else
                        ( { model
                            | ball = updateBallPosition frame model.ball
                            , ballPath = List.take 99 <| model.ball :: model.ballPath
                            , rightPaddle = updateAIPaddlePosition frame model.ball model.rightPaddle
                          }
                        , Cmd.none
                        )

                End ->
                    ( model, Cmd.none )

        PlayerCheckedBallPathToggle ballPathToggleValue ->
            ( { model | ballPathToggle = ballPathToggleValue }, Cmd.none )

        PlayerPressedKeyDown key ->
            ( updateKeyPress key model, Cmd.none )

        PlayerPressedKeyUp _ ->
            ( clearKeyPresses model, Cmd.none )



-- PREDICATES


ballHitTopEdge : Ball -> Bool
ballHitTopEdge ball =
    (ball.y - ball.height) <= 0


ballHitBottomEdge : Ball -> Window -> Bool
ballHitBottomEdge ball window_ =
    (ball.y + ball.height) >= window_.height


ballHitLeftEdge : Ball -> Bool
ballHitLeftEdge ball =
    (ball.x - ball.width) <= 0


ballHitRightEdge : Ball -> Window -> Bool
ballHitRightEdge ball window_ =
    (ball.x + ball.width) >= window_.width


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



-- OTHER


updateBallPosition : Float -> Ball -> Ball
updateBallPosition frame ball =
    { ball
        | x = round <| toFloat ball.x + ball.vx * frame
        , y = round <| toFloat ball.y + ball.vy * frame
    }


updateAIPaddlePosition : Float -> Ball -> Paddle -> Paddle
updateAIPaddlePosition frame ball paddle =
    if ball.y > paddle.y then
        { paddle | y = keepPaddleInWindow paddle <| paddle.y + 5 }

    else if ball.y < paddle.y then
        { paddle | y = keepPaddleInWindow paddle <| paddle.y - 5 }

    else
        paddle


keepPaddleInWindow : Paddle -> Int -> Int
keepPaddleInWindow paddle paddleYPosition =
    let
        topEdge =
            0

        bottomEdge =
            initialWindow.height - paddle.height
    in
    Basics.clamp topEdge bottomEdge paddleYPosition


movePaddleDown : Paddle -> Paddle
movePaddleDown paddle =
    { paddle | y = keepPaddleInWindow paddle <| paddle.y + 10 }


movePaddleUp : Paddle -> Paddle
movePaddleUp paddle =
    { paddle | y = keepPaddleInWindow paddle <| paddle.y - 10 }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta GameLoop
        , Browser.Events.onKeyDown <| Json.Decode.map PlayerPressedKeyDown <| keyDecoder
        , Browser.Events.onKeyUp <| Json.Decode.map PlayerPressedKeyUp <| keyDecoder
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
                Start ->
                    Html.div []
                        [ viewSvg model
                        , viewInstructions
                        , viewOptions model.ballPathToggle
                        ]

                Playing ->
                    Html.div []
                        [ viewSvg model
                        , viewInstructions
                        , viewOptions model.ballPathToggle
                        ]

                End ->
                    Html.div []
                        [ viewSvg model
                        , viewWinner model.winner
                        , viewInstructions
                        , viewOptions model.ballPathToggle
                        ]
            ]
        ]


viewSvg : Model -> Svg.Svg msg
viewSvg model =
    let
        viewBoxString =
            [ initialWindow.x
            , initialWindow.y
            , initialWindow.width
            , initialWindow.height
            ]
                |> List.map String.fromInt
                |> String.join " "
    in
    Svg.svg
        [ Svg.Attributes.viewBox viewBoxString
        , Svg.Attributes.width <| String.fromInt initialWindow.width
        , Svg.Attributes.height <| String.fromInt initialWindow.height
        ]
        ([ viewGameWindow initialWindow
         , viewNet initialWindow
         , viewLeftPaddleScore model.leftPaddleScore initialWindow
         , viewRightPaddleScore model.rightPaddleScore initialWindow
         , viewLeftPaddle model.leftPaddle
         , viewRightPaddle model.rightPaddle
         , viewBall model.ball
         ]
            ++ (if model.ballPathToggle then
                    viewBallPath model.ballPath

                else
                    [ Html.span [] [] ]
               )
        )


viewGameWindow : Window -> Svg.Svg msg
viewGameWindow window_ =
    Svg.rect
        [ Svg.Attributes.fill window_.backgroundColor
        , Svg.Attributes.x <| String.fromInt window_.x
        , Svg.Attributes.y <| String.fromInt window_.y
        , Svg.Attributes.width <| String.fromInt window_.width
        , Svg.Attributes.height <| String.fromInt window_.height
        ]
        []


viewNet : Window -> Svg.Svg msg
viewNet window_ =
    Svg.line
        [ Svg.Attributes.stroke "white"
        , Svg.Attributes.strokeDasharray "30, 15"
        , Svg.Attributes.strokeWidth "8"
        , Svg.Attributes.x1 <| String.fromInt <| (window_.width // 2 + 10)
        , Svg.Attributes.x2 <| String.fromInt <| (window_.width // 2 + 10)
        , Svg.Attributes.y1 "0"
        , Svg.Attributes.y2 <| String.fromInt window_.height
        ]
        []


viewLeftPaddleScore : Int -> Window -> Svg.Svg msg
viewLeftPaddleScore score window_ =
    Svg.text_
        [ Svg.Attributes.fill "white"
        , Svg.Attributes.fontFamily "Courier New"
        , Svg.Attributes.fontSize "80"
        , Svg.Attributes.fontWeight "bold"
        , Svg.Attributes.x <| String.fromInt <| (window_.width // 2) - 200
        , Svg.Attributes.y "100"
        ]
        [ Svg.text <| String.fromInt score ]


viewRightPaddleScore : Int -> Window -> Svg.Svg msg
viewRightPaddleScore score window_ =
    Svg.text_
        [ Svg.Attributes.fill "white"
        , Svg.Attributes.fontFamily "Courier New"
        , Svg.Attributes.fontSize "80"
        , Svg.Attributes.fontWeight "bold"
        , Svg.Attributes.x <| String.fromInt <| (window_.width // 2) + 200
        , Svg.Attributes.y "100"
        ]
        [ Svg.text <| String.fromInt score ]


viewLeftPaddle : Paddle -> Svg.Svg msg
viewLeftPaddle paddle =
    Svg.rect
        [ Svg.Attributes.fill paddle.color
        , Svg.Attributes.x <| String.fromInt <| paddle.x
        , Svg.Attributes.y <| String.fromInt <| paddle.y
        , Svg.Attributes.width <| String.fromInt <| paddle.width
        , Svg.Attributes.height <| String.fromInt <| paddle.height
        ]
        []


viewRightPaddle : Paddle -> Svg.Svg msg
viewRightPaddle paddle =
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


paddleIdToString : PaddleId -> String
paddleIdToString paddleId =
    case paddleId of
        Left ->
            "Left"

        Right ->
            "Right"


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


viewOptions : Bool -> Html.Html Msg
viewOptions ballPathToggle =
    Html.div [ Html.Attributes.class "pt-2" ]
        [ Html.h2 [ Html.Attributes.class "font-bold font-gray-800 pb-1 text-xl" ]
            [ Html.text "Options" ]
        , viewBallPathToggle ballPathToggle
        ]


viewBallPathToggle : Bool -> Html.Html Msg
viewBallPathToggle ballPathToggle =
    Html.div []
        [ Html.input
            [ Html.Attributes.checked ballPathToggle
            , Html.Attributes.class "mx-2"
            , Html.Attributes.id "ball-path-toggle"
            , Html.Attributes.type_ "checkbox"
            , Html.Events.onCheck <| PlayerCheckedBallPathToggle
            ]
            []
        , Html.label
            [ Html.Attributes.class "font-medium italic"
            , Html.Attributes.for "ball-path-toggle"
            ]
            [ Html.text "View ball path history?" ]
        ]



-- PORTS


port playSound : Json.Encode.Value -> Cmd msg
