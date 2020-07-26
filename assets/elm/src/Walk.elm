module Walk exposing
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
import Random exposing (Generator)
import Svg exposing (Svg)
import Svg.Attributes
import Walk.Window exposing (Window)



-- MODEL


type alias Character =
    { direction : Direction
    , position : ( Float, Float )
    }


type Direction
    = East
    | North
    | South
    | West


type alias Model =
    Character


type alias Time =
    Float


type Turn
    = Left
    | Right



-- INIT


initialCharacter : Character
initialCharacter =
    { direction = South
    , position =
        ( Walk.Window.globalWindow.width / 2
        , Walk.Window.globalWindow.height / 2
        )
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialCharacter, Cmd.none )



-- UPDATE


type Msg
    = BrowserAdvancedAnimationFrame Time
    | GeneratedRandomTurn Turn


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BrowserAdvancedAnimationFrame _ ->
            ( updateCharacterPosition model, generateRandomTurn )

        GeneratedRandomTurn turn ->
            ( updateCharacterDirection turn model, Cmd.none )



-- UPDATES


updateCharacterPosition : Character -> Character
updateCharacterPosition character =
    let
        ( x, y ) =
            character.position
    in
    case character.direction of
        East ->
            { character | position = ( x + 1, y ) }

        North ->
            { character | position = ( x, y - 1 ) }

        South ->
            { character | position = ( x, y + 1 ) }

        West ->
            { character | position = ( x - 1, y ) }


updateCharacterDirection : Turn -> Character -> Character
updateCharacterDirection turn character =
    case ( turn, character.direction ) of
        ( Left, East ) ->
            { character | direction = North }

        ( Left, North ) ->
            { character | direction = West }

        ( Left, South ) ->
            { character | direction = East }

        ( Left, West ) ->
            { character | direction = South }

        ( Right, East ) ->
            { character | direction = South }

        ( Right, North ) ->
            { character | direction = East }

        ( Right, South ) ->
            { character | direction = West }

        ( Right, West ) ->
            { character | direction = North }



-- COMMANDS


randomTurnGenerator : Generator Turn
randomTurnGenerator =
    Random.uniform Left [ Right ]


generateRandomTurn : Cmd Msg
generateRandomTurn =
    Random.generate GeneratedRandomTurn randomTurnGenerator



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta BrowserAdvancedAnimationFrame



-- VIEW


view : Model -> Document Msg
view model =
    { title = "🏴\u{200D}☠️ Random Walk"
    , body = [ viewMain model ]
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
        [ Html.h1 [] [ Html.text "Random Walk" ] ]


viewGame : Model -> Html Msg
viewGame model =
    Html.section [ Html.Attributes.class "flex justify-center my-4" ]
        [ Html.div [ Html.Attributes.class "flex-shrink-0" ]
            [ viewSvg Walk.Window.globalWindow model ]
        ]


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
        [ Walk.Window.viewGameWindow window
        , viewCharacter model
        ]


viewCharacter : Character -> Svg msg
viewCharacter character =
    let
        ( x, y ) =
            character.position
    in
    Svg.rect
        [ Svg.Attributes.fill "white"
        , Svg.Attributes.x <| String.fromFloat x
        , Svg.Attributes.y <| String.fromFloat y
        , Svg.Attributes.width "2"
        , Svg.Attributes.height "2"
        ]
        []
