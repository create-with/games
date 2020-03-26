module Main exposing (main)

-- IMPORTS

import Browser
import Pong.Main



-- MAIN


main : Program () Pong.Main.Model Pong.Main.Msg
main =
    Browser.element
        { init = Pong.Main.init
        , update = Pong.Main.update
        , subscriptions = Pong.Main.subscriptions
        , view = Pong.Main.view
        }
