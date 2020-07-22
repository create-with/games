module Main exposing (main)

-- IMPORTS

import App
import Browser



-- MAIN


main : Program App.Flags App.Model App.Msg
main =
    Browser.application
        { init = App.init
        , onUrlChange = App.ChangedUrl
        , onUrlRequest = App.ClickedUrl
        , subscriptions = App.subscriptions
        , update = App.update
        , view = App.view
        }
