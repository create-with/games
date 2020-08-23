module Landing exposing (view)

-- IMPORTS

import Browser
import Html exposing (Html)
import Html.Attributes
import Util.View



-- MODEL


type alias Game =
    { color : String
    , emoji : String
    , slug : String
    , title : String
    }


allGames : List Game
allGames =
    [ { color = "yellow"
      , emoji = "\u{1F3D3}"
      , slug = "pong"
      , title = "Pong"
      }
    , { color = "blue"
      , emoji = "\u{1F6F8}"
      , slug = "breakout"
      , title = "Breakout"
      }
    ]



-- VIEW


view : Browser.Document msg
view =
    { title = "🕹 Games"
    , body =
        [ Html.main_ [ Html.Attributes.class "flex flex-col min-h-screen" ]
            [ viewGames allGames
            , Util.View.footer
            ]
        ]
    }


viewGames : List Game -> Html msg
viewGames games =
    Html.section [ Html.Attributes.class "flex-grow" ]
        [ games
            |> List.map viewGame
            |> Html.ul [ Html.Attributes.class "px-16 py-8" ]
        ]


viewGame : Game -> Html msg
viewGame { color, emoji, slug, title } =
    Html.a [ Html.Attributes.href slug ]
        [ Html.li
            [ Html.Attributes.class <| colorToBorderClass color
            , Html.Attributes.class <| colorToColorClass color
            , Html.Attributes.class "hover:shadow-lg max-w-sm my-4 px-6 py-3 rounded-md shadow"
            ]
            [ Html.span [ Html.Attributes.class "mr-2" ]
                [ Html.text emoji ]
            , Html.strong []
                [ Html.text title ]
            ]
        ]


colorToBorderClass : String -> String
colorToBorderClass color =
    "border-b-2 border-solid border-" ++ color ++ "-700"


colorToColorClass : String -> String
colorToColorClass color =
    "bg-" ++ color ++ "-200"
