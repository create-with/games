module Util.View exposing
    ( footer
    , radioButton
    )

-- IMPORTS

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Util.Icon



-- FOOTER


footer : Html a
footer =
    Html.footer [ Html.Attributes.class "bg-black font-semibold px-24 py-12 text-sm text-white w-full" ]
        [ Html.ul [ Html.Attributes.class "flex justify-evenly" ]
            [ Html.a
                [ Html.Attributes.class ""
                , Html.Attributes.href "https://github.com/create-with/games"
                , Html.Attributes.target "_blank"
                ]
                [ Html.li [ Html.Attributes.class "flex" ]
                    [ Html.span [ Html.Attributes.class "self-center" ]
                        [ Util.Icon.github ]
                    , Html.span [ Html.Attributes.class "self-center" ]
                        [ Html.text "GitHub" ]
                    ]
                ]
            , Html.li [ Html.Attributes.class "" ] [ Html.text "Blog Posts" ]
            , Html.li [ Html.Attributes.class "" ] [ Html.text "Credits" ]
            , Html.li [ Html.Attributes.class "" ] [ Html.text "Follow" ]
            ]
        ]



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
