module Mario exposing
    ( Model
    , Msg(..)
    , init
    , subscriptions
    , update
    , view
    )

-- IMPORTS

import Browser exposing (Document)
import Html exposing (Html)
import Html.Attributes
import Util.View



-- MODEL


type alias Model =
    { mario : ( Int, Int ) }



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { mario = ( 0, 0 ) }, Cmd.none )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : (Msg -> msg) -> Model -> Document msg
view msg model =
    { title = "🍄 Mario"
    , body = List.map (Html.map msg) [ viewMain model, Util.View.footer ]
    }


viewMain : Model -> Html Msg
viewMain _ =
    Html.main_ [ Html.Attributes.class "bg-red-400 h-full p-8" ]
        [-- viewHeader
         -- , viewGame model
         -- , viewInformation model
        ]
