module RandomWalk exposing
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
import RandomWalk.Window exposing (Window)
import Set
import Svg exposing (Svg)
import Svg.Attributes
import Util.Fps exposing (ShowFps, Time)
import Util.Keyboard exposing (Controls)
import Util.View



-- MODEL


type alias Model =
    { deltaTime : Time
    , deltaTimes : List Time
    , playerKeyPress : Controls
    , showFps : ShowFps
    , walker : Walker
    }


type alias Walker =
    { x : Int
    , y : Int
    }



-- INIT


initialModel : Model
initialModel =
    { deltaTime = 0.0
    , deltaTimes = []
    , playerKeyPress = Util.Keyboard.initialKeys
    , showFps = Util.Fps.On
    , walker = initialWalker
    }


initialWalker : Walker
initialWalker =
    { x = 0
    , y = 0
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
        BrowserAdvancedAnimationFrame _ ->
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
    { title = "🍻 Random Walk"
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
            [ Html.text "Random Walk" ]
        ]


viewGame : Model -> Html Msg
viewGame model =
    Html.section [ Html.Attributes.class "flex flex-row my-4" ]
        [ viewSvg RandomWalk.Window.globalWindow model ]


viewSvg : Window -> Model -> Svg msg
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
        ]
        [ RandomWalk.Window.viewGameWindow window
        , Util.Fps.viewFps model.showFps model.deltaTimes
        ]
