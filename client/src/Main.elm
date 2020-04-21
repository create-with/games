module Main exposing (main)

-- IMPORTS

import Breakout
import Browser
import Pong



-- MAIN


main : Program () Pong.Model Pong.Msg
main =
    Browser.document
        { init = Pong.init
        , update = Pong.update
        , subscriptions = Pong.subscriptions
        , view = Pong.view
        }



-- BREAKOUT
-- main : Program () Breakout.Model Breakout.Msg
-- main =
--     Browser.document
--         { init = Breakout.init
--         , update = Breakout.update
--         , subscriptions = Breakout.subscriptions
--         , view = Breakout.view
--         }
