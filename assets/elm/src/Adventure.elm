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
import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3)
import Set
import Util.Fps exposing (Time)
import Util.Keyboard exposing (Controls)
import Util.View
import WebGL exposing (Mesh, Shader)



-- MODEL


type alias Model =
    { deltaTime : Time
    , playerKeyPress : Controls
    }



-- INIT


initialModel : Model
initialModel =
    { deltaTime = 0.0
    , playerKeyPress = Util.Keyboard.initialKeys
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
            ( { model | deltaTime = model.deltaTime + 10.0 }, Cmd.none )

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
    Html.main_
        [ Html.Attributes.class "bg-yellow-200 h-full p-8" ]
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
    Html.section [ Html.Attributes.class "flex justify-center my-4" ]
        [ WebGL.toHtml
            [ Html.Attributes.height 400
            , Html.Attributes.width 600
            , Html.Attributes.style "display" "block"
            ]
            [ WebGL.entity
                vertexShader
                fragmentShader
                mesh
                { perspective = perspective (model.deltaTime / 1000) }
            ]
        ]


perspective : Float -> Mat4
perspective t =
    Math.Matrix4.mul
        (Math.Matrix4.makePerspective 45 1 0.01 100)
        (Math.Matrix4.makeLookAt (Math.Vector3.vec3 (4 * cos t) 0 (4 * sin t)) (Math.Vector3.vec3 0 0 0) (Math.Vector3.vec3 0 1 0))



-- Mesh


type alias Vertex =
    { position : Vec3
    , color : Vec3
    }


mesh : Mesh Vertex
mesh =
    WebGL.triangles
        [ ( Vertex (Math.Vector3.vec3 0 0 0) (Math.Vector3.vec3 1 0 0)
          , Vertex (Math.Vector3.vec3 1 1 0) (Math.Vector3.vec3 0 1 0)
          , Vertex (Math.Vector3.vec3 1 -1 0) (Math.Vector3.vec3 0 0 1)
          )
        ]



-- Shaders


type alias Uniforms =
    { perspective : Mat4 }


vertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        varying vec3 vcolor;
        void main () {
            gl_Position = perspective * vec4(position, 1.0);
            vcolor = color;
        }
    |]


fragmentShader : Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|
        precision mediump float;
        varying vec3 vcolor;
        void main () {
            gl_FragColor = vec4(vcolor, 1.0);
        }
    |]
