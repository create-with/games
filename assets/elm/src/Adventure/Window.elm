module Adventure.Window exposing
    ( Window
    , initialWindow
    , view
    )

-- IMPORTS

import Svg exposing (Svg)
import Svg.Attributes



-- MODEL


type alias Window =
    { backgroundColor : String
    , x : Float
    , y : Float
    , width : Float
    , height : Float
    }


initialWindow : Window
initialWindow =
    { backgroundColor = "lightgray"
    , x = 0.0
    , y = 0.0
    , width = 800.0
    , height = 600.0
    }



-- VIEW


view : Window -> Svg msg
view window =
    Svg.rect
        [ Svg.Attributes.fill <| window.backgroundColor
        , Svg.Attributes.x <| String.fromFloat window.x
        , Svg.Attributes.y <| String.fromFloat window.y
        , Svg.Attributes.width <| String.fromFloat window.width
        , Svg.Attributes.height <| String.fromFloat window.height
        ]
        []
