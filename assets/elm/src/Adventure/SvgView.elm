module Adventure.SvgView exposing (view)

-- IMPORTS

import Adventure.Character exposing (Character)
import Adventure.Screen exposing (Screen)
import Adventure.Window exposing (Window)
import Svg exposing (Svg)
import Svg.Attributes



-- 2D VIEW


view : Window -> Screen -> Character -> Svg a
view window screen character =
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
        [ Adventure.Window.view window
        , Adventure.Screen.view screen
        , Adventure.Character.view character
        ]
