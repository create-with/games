port module Tetris exposing
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


type GameState
    = StartingScreen
    | PlayingScreen
    | EndingScreen

type alias Player =
    { x : Int
    , y : Int
    , matrix : Matrix
    , score : Int
    }


initialPlayer : Player
initialPlayer =
    { x = 0
    , y = 0
    , matrix = initialMatrix
    , score = 0
    }

type alias Matrix =
    List (List Int)

initialMatrix : Matrix
initialMatrix =
    [
        [ 0, 0, 0 ]
        , [1, 1, 1]
        , [0, 1, 0]
    ]

type Shapes
    = A
    | B
    | C
    | D
    | E
    | F
    | G


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

type alias Arena =
    { backgroundColor : String
    , x : Int
    , y : Int
    , width : Int
    , height : Int
    }

initialArena : Arena
initialArena =
    { backgroundColor = "darkgray"
    , x = 0
    , y = 0
    , width = 360
    , height = 600
    }


type alias Model =
    { arena : Arena
    , gameState : GameState
    , matrix : Matrix
    , player : Player
    , playerKeyPress : Set.Set String
    }


initialModel : Model
initialModel =
    { arena = initialArena
    , gameState = StartingScreen
    , matrix = initialMatrix
    , player = initialPlayer
    , playerKeyPress = Set.empty
    }


initialCommand : Cmd Msg
initialCommand =
    Cmd.none


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, initialCommand )



-- UPDATE


type Msg
    = BrowserAdvancedAnimationFrame GameState Float
    | PlayerPressedKeyDown String
    | PlayerReleasedKey String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BrowserAdvancedAnimationFrame StartingScreen _ ->
            if playerPressedSpacebarKey model.playerKeyPress then
                ( { model | gameState = PlayingScreen }, Cmd.none)

            else
                (model, Cmd.none)

        BrowserAdvancedAnimationFrame PlayingScreen time ->
            (model, Cmd.none)

        BrowserAdvancedAnimationFrame EndingScreen _ ->
            (model, Cmd.none)

        PlayerPressedKeyDown key ->
            ( updateKeyPress key model, Cmd.none)

        PlayerReleasedKey _ ->
            ( clearKeyPresses model, Cmd.none )





-- COMMANDS




playSoundCommand : String -> Model -> ( Model, Cmd Msg )
playSoundCommand soundFile model =
    ( model, playSound <| Json.Encode.string soundFile )



-- UPDATE HELPERS




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ browserAnimationSubscription model.gameState
        , keyDownSubscription
        , keyUpSubscription
        ]


browserAnimationSubscription : GameState -> Sub Msg
browserAnimationSubscription gameState =
    Browser.Events.onAnimationFrameDelta <| BrowserAdvancedAnimationFrame gameState


keyDownSubscription : Sub Msg
keyDownSubscription =
    Browser.Events.onKeyDown <| Json.Decode.map PlayerPressedKeyDown <| keyDecoder


keyUpSubscription : Sub Msg
keyUpSubscription =
    Browser.Events.onKeyUp <| Json.Decode.map PlayerReleasedKey <| keyDecoder



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
            [ Html.h1 [ Html.Attributes.class "font-black text-black text-5xl" ]
                [ Html.text "🏰 Tetris " ]
            ]
        , Html.section []
            [ case model.gameState of
                StartingScreen ->
                    Html.div []
                        [ viewSvg globalWindow model
                        , viewInstructions
                        , viewOptions
                        ]

                PlayingScreen ->
                    Html.div []
                        [ viewSvg globalWindow model
                        , viewInstructions
                        , viewOptions
                        ]

                EndingScreen ->
                    Html.div []
                        [ viewSvg globalWindow model
                        , viewInstructions
                        , viewOptions
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
        [ viewGameWindow window ]


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



viewInstructions : Html.Html msg
viewInstructions =
    Html.div [ Html.Attributes.class "pt-2" ]
        [ Html.h2 [ Html.Attributes.class "font-bold font-gray-800 pb-1 text-xl" ]
            [ Html.text "Instructions" ]
        , Html.ul [ Html.Attributes.class "list-disc list-inside mx-3" ]
            [ Html.li [] [ Html.text "\u{1F3D3} Press the SPACEBAR key to start." ]
            , Html.li [] [ Html.text "⌨️ Use the arrow keys to move." ]
            ]
        ]


viewOptions : Html.Html Msg
viewOptions =
    Html.div [ Html.Attributes.class "pt-2" ]
        [ Html.h2 [ Html.Attributes.class "font-bold font-gray-800 pb-1 text-xl" ]
            [ Html.text "Options" ]
        , Html.form [] []
        ]


-- PORTS


port playSound : Json.Encode.Value -> Cmd msg
