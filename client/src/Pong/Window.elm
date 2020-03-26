module Pong.Window exposing
    ( Window
    , WindowEdge(..)
    , window
    )

-- MODEL


type alias Window =
    { backgroundColor : String
    , x : Int
    , y : Int
    , width : Int
    , height : Int
    }


type WindowEdge
    = Top
    | Bottom



-- GLOBAL


window : Window
window =
    { backgroundColor = "black"
    , x = 0
    , y = 0
    , width = 800
    , height = 600
    }
