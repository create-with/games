module Main exposing (main)

-- IMPORTS

import Browser
import Pong


-- MAIN


main : Program () Pong.Model Pong.Msg
main =
    Browser.element
        { init = Pong.init
        , update = Pong.update
        , subscriptions = Pong.subscriptions
        , view = Pong.view
        }
