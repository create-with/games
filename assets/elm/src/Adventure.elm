module Adventure exposing
    ( Model
    , Msg(..)
    , init
    , subscriptions
    , update
    , view
    )

-- IMPORTS

import Adventure.Character exposing (Character)
import Adventure.Screen exposing (Screen)
import Adventure.SvgView
import Adventure.WebGLView
import Adventure.Window exposing (Window)
import Browser exposing (Document)
import Browser.Events
import Html exposing (Html)
import Html.Attributes
import Json.Decode
import Set
import Util.Fps exposing (Time)
import Util.Keyboard exposing (Controls)
import Util.View



-- MODEL


type alias Model =
    { character : Character
    , deltaTime : Time
    , playerKeyPress : Controls
    , screen : Screen
    , window : Window
    }



-- INIT


initialModel : Model
initialModel =
    { character = Adventure.Character.initialCharacter
    , deltaTime = 0.0
    , playerKeyPress = Util.Keyboard.initialKeys
    , screen = Adventure.Screen.screen01
    , window = Adventure.Window.initialWindow
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
    | PlayerPressedKeyDown String
    | PlayerReleasedKey String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BrowserAdvancedAnimationFrame deltaTime ->
            ( { model | character = updateCharacter model.playerKeyPress deltaTime model.character }
            , Cmd.none
            )

        PlayerPressedKeyDown key ->
            ( updateKeyPress key model, Cmd.none )

        PlayerReleasedKey _ ->
            ( { model | playerKeyPress = Set.empty }, Cmd.none )


updateCharacter : Controls -> Time -> Character -> Character
updateCharacter playerKeyPress deltaTime character =
    if Util.Keyboard.playerPressedArrowUpKey playerKeyPress then
        { character | y = character.y - character.vy * deltaTime }

    else if Util.Keyboard.playerPressedArrowDownKey playerKeyPress then
        { character | y = character.y + character.vy * deltaTime }

    else if Util.Keyboard.playerPressedArrowLeftKey playerKeyPress then
        { character | x = character.x - character.vx * deltaTime }

    else if Util.Keyboard.playerPressedArrowRightKey playerKeyPress then
        { character | x = character.x + character.vx * deltaTime }

    else
        character


updateKeyPress : String -> Model -> Model
updateKeyPress key model =
    if Set.member key Util.Keyboard.validKeys then
        { model | playerKeyPress = Set.insert key model.playerKeyPress }

    else
        model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ browserAnimationSubscription
        , keyDownSubscription
        , keyUpSubscription
        ]


browserAnimationSubscription : Sub Msg
browserAnimationSubscription =
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



-- VIEW


view : (Msg -> msg) -> Model -> Document msg
view msg model =
    { title = "⚔️ Adventure"
    , body = List.map (Html.map msg) [ viewMain model, Util.View.footer ]
    }


viewMain : Model -> Html Msg
viewMain model =
    Html.main_
        [ Html.Attributes.class "h-full p-8"
        , Html.Attributes.style "background-color" "lightgray"
        ]
        [ viewHeader
        , viewGame model
        ]


viewHeader : Html msg
viewHeader =
    Html.header [ Html.Attributes.class "flex justify-center" ]
        [ Html.h1 [ Html.Attributes.class "font-black text-5xl" ]
            [ Html.text "Adventure" ]
        ]


viewGame : Model -> Html Msg
viewGame model =
    Html.section [ Html.Attributes.class "flex flex-row my-4" ]
        [ Adventure.SvgView.view model.window model.screen model.character
        , Adventure.WebGLView.view model.window
        ]
