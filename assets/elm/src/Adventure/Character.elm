module Adventure.Character exposing
    ( Character
    , initialCharacter
    , view
    )

-- IMPORTS

import Svg exposing (Svg)
import Svg.Attributes



-- MODEL


type alias Character =
    { color : String
    , x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , width : Float
    , height : Float
    }


initialCharacter : Character
initialCharacter =
    { color = "yellow"
    , x = 395.0
    , y = 500.0
    , vx = 350.0
    , vy = 350.0
    , width = 10.0
    , height = 10.0
    }



-- VIEW


view : Character -> Svg a
view character =
    Svg.rect
        [ Svg.Attributes.fill <| character.color
        , Svg.Attributes.x <| String.fromFloat character.x
        , Svg.Attributes.y <| String.fromFloat character.y
        , Svg.Attributes.width <| String.fromFloat character.width
        , Svg.Attributes.height <| String.fromFloat character.height
        ]
        []
