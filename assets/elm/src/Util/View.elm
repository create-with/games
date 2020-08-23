module Util.View exposing
    ( footer
    , radioButton
    )

-- IMPORTS

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Svg exposing (Svg)
import Util.Icon



-- FOOTER


footer : Html a
footer =
    Html.footer [ Html.Attributes.class "bg-black bottom-0 font-semibold px-24 py-12 relative text-sm text-white w-full" ]
        [ Html.ul [ Html.Attributes.class "flex justify-evenly" ]
            [ footerItem "Home" "/" Util.Icon.home "pink"
            , footerItem "GitHub" "https://github.com/create-with/games" Util.Icon.github "purple"
            , footerItem "Twitter" "https://twitter.com/bijanbwb" Util.Icon.twitter "blue"
            , footerItem "Articles" "https://dev.to/bijanbwb/" Util.Icon.dev "yellow"
            , footerItem "Credits" "https://github.com/create-with/games/blob/master/CREDITS.md" Util.Icon.thumbsUp "teal"
            ]
        ]


footerItem : String -> String -> Svg a -> String -> Html a
footerItem title url icon color =
    Html.a
        [ Html.Attributes.href url
        , if title /= "Home" then
            Html.Attributes.target "_blank"

          else
            Html.Attributes.target "_self"
        ]
        [ Html.li [ Html.Attributes.class <| "flex hover:text-" ++ color ++ "-300 self-center" ]
            [ Html.span [ Html.Attributes.class "self-center" ] [ icon ]
            , Html.span [ Html.Attributes.class "self-center" ] [ Html.text title ]
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
