module Adventure.WebGLView exposing (view)

-- IMPORTS

import Adventure.Window exposing (Window)
import Html exposing (Html)
import Html.Attributes
import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3)
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Settings.StencilTest



-- CONFIG


type alias Config =
    { fps : Int
    , tps : Int
    , max : Int
    , spriteSize : Int
    }


config : Config
config =
    { fps = 60
    , tps = 5
    , max = 10
    , spriteSize = 20
    }



-- VIEW


view : Window -> Html a
view window =
    let
        ratio =
            window.width / window.height
    in
    Html.div []
        [ WebGL.toHtmlWith
            [ WebGL.alpha True
            , WebGL.antialias
            , WebGL.depth 1
            , WebGL.stencil 0
            ]
            [ Html.Attributes.height <| round window.height
            , Html.Attributes.width <| round window.width
            ]
            [ wallsView ratio ]
        ]


wallsView : Float -> Entity
wallsView ratio =
    WebGL.entityWith
        [ WebGL.Settings.StencilTest.test
            { ref = 1
            , mask = 0xFF
            , test = WebGL.Settings.StencilTest.always
            , fail = WebGL.Settings.StencilTest.keep
            , zfail = WebGL.Settings.StencilTest.keep
            , zpass = WebGL.Settings.StencilTest.replace
            , writeMask = 0xFF
            }
        ]
        vertexShader
        fragmentShader
        square
        (Uniforms (Math.Vector3.vec3 0.4 0.2 0.3)
            (Math.Vector3.vec3 0 0 0)
            (Math.Matrix4.makeScale3 (toFloat config.max + 1) (toFloat config.max + 1) 1
                |> Math.Matrix4.mul (Math.Matrix4.makeTranslate3 (toFloat config.max / 2) (toFloat config.max / 2) 0)
                |> Math.Matrix4.mul (camera ratio)
            )
            light
        )


camera : Float -> Mat4
camera ratio =
    let
        c =
            toFloat config.max / 2

        eye =
            Math.Vector3.vec3 c -c 15

        center =
            Math.Vector3.vec3 c c 0
    in
    Math.Matrix4.mul (Math.Matrix4.makePerspective 45 ratio 0.01 100)
        (Math.Matrix4.makeLookAt eye center Math.Vector3.j)


light : Vec3
light =
    Math.Vector3.vec3 -1 1 3
        |> Math.Vector3.normalize



-- perspective : Float -> Mat4
-- perspective t =
--     Math.Matrix4.mul
--         (Math.Matrix4.makePerspective 45 1 0.01 100)
--         (Math.Matrix4.makeLookAt (Math.Vector3.vec3 (4 * cos t) 0 (4 * sin t)) (Math.Vector3.vec3 0 0 0) (Math.Vector3.vec3 0 1 0))
-- SHADERS


type alias Uniforms =
    { color : Vec3
    , offset : Vec3
    , camera : Mat4
    , light : Vec3
    }


type alias Varying =
    { vlighting : Float
    }


vertexShader : Shader Attributes Uniforms Varying
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 normal;
        uniform vec3 offset;
        uniform mat4 camera;
        uniform vec3 light;
        varying highp float vlighting;
        void main () {
            highp float ambientLight = 0.5;
            highp float directionalLight = 0.5;
            gl_Position = camera * vec4(position + offset, 1.0);
            vlighting = ambientLight + max(dot(normal, light), 0.0) * directionalLight;
        }
    |]


fragmentShader : Shader {} Uniforms Varying
fragmentShader =
    [glsl|
        precision mediump float;
        varying highp float vlighting;
        uniform vec3 color;
        void main () {
            gl_FragColor = vec4(color * vlighting, 1.0);
        }
    |]



-- GEOMETRIES


type alias Attributes =
    { position : Vec3
    , normal : Vec3
    }


attributes : Vec3 -> Vec3 -> Vec3 -> ( Attributes, Attributes, Attributes )
attributes p1 p2 p3 =
    let
        normal =
            Math.Vector3.sub p1 p3
                |> Math.Vector3.cross (Math.Vector3.sub p1 p2)
                |> Math.Vector3.normalize

        attributesFor : Vec3 -> Attributes
        attributesFor p =
            Attributes p normal
    in
    ( attributesFor p1, attributesFor p2, attributesFor p3 )


square : Mesh Attributes
square =
    WebGL.triangles
        [ attributes (Math.Vector3.vec3 -0.5 0.5 0) (Math.Vector3.vec3 -0.5 -0.5 0) (Math.Vector3.vec3 0.5 0.5 0)
        , attributes (Math.Vector3.vec3 -0.5 -0.5 0) (Math.Vector3.vec3 0.5 -0.5 0) (Math.Vector3.vec3 0.5 0.5 0)
        ]
