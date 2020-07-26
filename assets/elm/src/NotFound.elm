module NotFound exposing (view)

-- IMPORTS

import Browser
import Html



-- VIEW


view : Browser.Document msg
view =
    { title = "Page Not Found"
    , body =
        [ Html.main_ []
            [ Html.p []
                [ Html.text "Not Found. Return from whence ye came! ⚔️" ]
            ]
        ]
    }
