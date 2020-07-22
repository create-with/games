module App exposing
    ( Flags
    , Model
    , Msg(..)
    , init
    , subscriptions
    , update
    , view
    )

-- IMPORTS

import Breakout
import Browser
import Browser.Navigation
import Landing
import NotFound
import Pong
import Route
import Url
import Url.Parser



-- MODEL


type alias Flags =
    ()


type alias Model =
    { flags : Flags
    , key : Browser.Navigation.Key
    , route : Route.Route
    , url : Url.Url
    }


init : Flags -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    changedUrl url <|
        { flags = flags
        , key = key
        , route = Route.NotFound
        , url = url
        }



-- UPDATE


type Msg
    = ChangedUrl Url.Url
    | ClickedUrl Browser.UrlRequest
    | ReceivedBreakoutMsg Breakout.Msg
    | ReceivedPongMsg Pong.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedUrl url ->
            changedUrl url model

        ClickedUrl urlRequest ->
            clickedUrl urlRequest model

        ReceivedBreakoutMsg pageMsg ->
            case model.route of
                Route.Breakout pageModel ->
                    changeToPage model Route.Breakout ReceivedBreakoutMsg <| Breakout.update pageMsg pageModel

                _ ->
                    ( model, Cmd.none )

        ReceivedPongMsg pageMsg ->
            case model.route of
                Route.Pong pageModel ->
                    changeToPage model Route.Pong ReceivedPongMsg <| Pong.update pageMsg pageModel

                _ ->
                    ( model, Cmd.none )


changedUrl : Url.Url -> Model -> ( Model, Cmd Msg )
changedUrl url model =
    case Url.Parser.parse (urlParser model) url of
        Just route ->
            route

        Nothing ->
            ( { model | route = Route.NotFound }, Cmd.none )


clickedUrl : Browser.UrlRequest -> Model -> ( Model, Cmd Msg )
clickedUrl urlRequest model =
    case urlRequest of
        Browser.Internal url ->
            ( model, Browser.Navigation.pushUrl model.key <| Url.toString url )

        Browser.External href ->
            ( model, Browser.Navigation.load href )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { route } =
    case route of
        Route.Breakout breakoutModel ->
            Sub.map ReceivedBreakoutMsg <| Breakout.subscriptions breakoutModel

        Route.Pong pongModel ->
            Sub.map ReceivedPongMsg <| Pong.subscriptions pongModel

        _ ->
            Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view { route } =
    case route of
        Route.Breakout breakoutModel ->
            Breakout.view ReceivedBreakoutMsg breakoutModel

        Route.Landing ->
            Landing.view

        Route.Pong pongModel ->
            Pong.view ReceivedPongMsg pongModel

        _ ->
            NotFound.view



-- ROUTING


urlParser : Model -> Url.Parser.Parser (( Model, Cmd Msg ) -> b) b
urlParser model =
    Url.Parser.oneOf
        [ landingPageParser model
        , pageParser model "breakout" Route.Breakout ReceivedBreakoutMsg <| Breakout.init model.flags
        , pageParser model "pong" Route.Pong ReceivedPongMsg <| Pong.init model.flags
        ]



-- PARSERS


pageParser : Model -> String -> (pageModel -> Route.Route) -> (pageMsg -> Msg) -> ( pageModel, Cmd pageMsg ) -> Url.Parser.Parser (( Model, Cmd Msg ) -> b) b
pageParser model urlString pageRoute pageMsg ( pageModel, pageCommand ) =
    urlString
        |> Url.Parser.s
        |> Url.Parser.map (changeToPage model pageRoute pageMsg ( pageModel, pageCommand ))


landingPageParser : Model -> Url.Parser.Parser (( Model, Cmd Msg ) -> b) b
landingPageParser model =
    Url.Parser.top
        |> Url.Parser.map (changeToLandingPage model)



-- PAGE CHANGES


changeToPage : Model -> (pageModel -> Route.Route) -> (pageMsg -> Msg) -> ( pageModel, Cmd pageMsg ) -> ( Model, Cmd Msg )
changeToPage model pageRoute pageMsg ( pageModel, pageCommand ) =
    ( { model | route = pageRoute pageModel }, Cmd.map pageMsg pageCommand )


changeToLandingPage : Model -> ( Model, Cmd Msg )
changeToLandingPage model =
    ( { model | route = Route.Landing }, Cmd.none )
