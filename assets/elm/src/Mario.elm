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
import Svg exposing (Svg)
import Svg.Attributes
import Util.View



-- MODEL


type alias Model =
    { mario : ( Int, Int )
    , window : Window
    }


type alias Window =
    { backgroundColor : String
    , x : Float
    , y : Float
    , width : Float
    , height : Float
    }



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( { mario = ( 0, 0 )
      , window = initialWindow
      }
    , Cmd.none
    )


initialWindow : Window
initialWindow =
    { backgroundColor = "black"
    , x = 0.0
    , y = 0.0
    , width = 256.0
    , height = 240.0
    }



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
viewMain model =
    Html.main_ [ Html.Attributes.class "bg-red-400 h-full p-8" ]
        [ viewHeader
        , viewGame model

        -- , viewInformation model
        ]


viewHeader : Html msg
viewHeader =
    Html.header [ Html.Attributes.class "flex justify-center" ]
        [ Html.h1 [ Html.Attributes.class "font-black text-5xl" ]
            [ Html.text "Mario" ]
        ]


viewGame : Model -> Html Msg
viewGame model =
    Html.section [ Html.Attributes.class "flex justify-center my-4" ]
        [ viewSvg model ]


viewSvg : Model -> Svg Msg
viewSvg { window } =
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
        , Svg.Attributes.width <| String.fromFloat <| window.width * 3
        , Svg.Attributes.height <| String.fromFloat <| window.height * 3
        ]
        [ viewGameWindow window
        ]


viewGameWindow : Window -> Svg msg
viewGameWindow window =
    Svg.rect
        [ Svg.Attributes.fill <| window.backgroundColor
        , Svg.Attributes.x <| String.fromFloat window.x
        , Svg.Attributes.y <| String.fromFloat window.y
        , Svg.Attributes.width <| String.fromFloat window.width
        , Svg.Attributes.height <| String.fromFloat window.height
        ]
        []
