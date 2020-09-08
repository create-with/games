module Adventure exposing
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
import Set
import Util.Fps exposing (Time)
import Util.Keyboard exposing (Controls)
import Util.View



-- MODEL


type alias Model =
    { playerKeyPress : Controls
    }



-- INIT


initialModel : Model
initialModel =
    { playerKeyPress = Util.Keyboard.initialKeys
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
            ( model, Cmd.none )

        PlayerPressedKeyDown key ->
            ( updateKeyPress key model, Cmd.none )

        PlayerReleasedKey _ ->
            ( { model | playerKeyPress = Set.empty }, Cmd.none )


updateKeyPress : String -> Model -> Model
updateKeyPress key model =
    if Set.member key Util.Keyboard.validKeys then
        { model | playerKeyPress = Set.insert key model.playerKeyPress }

    else
        model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
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
    Html.main_ [ Html.Attributes.class "bg-yellow-200 h-full p-8" ]
        [ viewHeader
        , viewGame model
        ]


viewHeader : Html msg
viewHeader =
    Html.header [ Html.Attributes.class "flex justify-center" ]
        [ Html.h1 [] [ Html.text "Adventure" ] ]


viewGame : Model -> Html Msg
viewGame model =
    Html.section [ Html.Attributes.class "flex justify-center my-4" ]
        [ Html.text "Game goes here..." ]
