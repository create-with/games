module NotFound exposing (view)

-- IMPORTS

import Browser
import Html
import Html.Attributes
import Util.View



-- VIEW


view : Browser.Document msg
view =
    { title = "Page Not Found"
    , body =
        [ Html.main_ [ Html.Attributes.class "flex flex-col min-h-screen" ]
            [ Html.p [ Html.Attributes.class "flex-grow font-bold my-64 text-center" ]
                [ Html.text "Page not found. Return from whence ye came! ⚔️" ]
            , Util.View.footer
            ]
        ]
    }
