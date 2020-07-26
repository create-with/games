module Landing exposing (view)

-- IMPORTS

import Browser
import Html exposing (Html)
import Html.Attributes



-- MODEL


type alias Game =
    { emoji : String
    , slug : String
    , title : String
    }



-- VIEW


view : Browser.Document msg
view =
    { title = "🕹 Elm Games"
    , body =
        [ Html.main_ [ Html.Attributes.class "p-12" ]
            [ Html.ul []
                [ viewGame <| Game "\u{1F3D3}" "pong" "Pong"
                , viewGame <| Game "\u{1F9F1}" "breakout" "Breakout"
                ]
            ]
        ]
    }


viewGame : Game -> Html msg
viewGame { emoji, slug, title } =
    Html.li [ Html.Attributes.class "my-2" ]
        [ Html.a [ Html.Attributes.href slug ]
            [ Html.span [ Html.Attributes.class "mr-1" ]
                [ Html.text emoji ]
            , Html.strong []
                [ Html.text title ]
            ]
        ]
