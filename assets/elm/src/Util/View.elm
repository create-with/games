module Util.View exposing (radioButton)

-- IMPORTS

import Html exposing (Html)
import Html.Attributes
import Html.Events



-- RADIO BUTTON


radioButton : a -> a -> (a -> String) -> (a -> msg) -> Html msg
radioButton type_ current toString msg =
    Html.label []
        [ Html.input
            [ Html.Attributes.checked <| current == type_
            , Html.Attributes.type_ "radio"
            , Html.Events.onClick <| msg type_
            ]
            []
        , Html.span [ Html.Attributes.class "px-1 text-xs" ]
            [ Html.text <| toString type_ ]
        ]
