module MarioPlayground exposing (..)

-- IMPORTS

import Playground exposing (..)



-- MAIN


main : Program () (Playground ()) Msg
main =
    picture
        [ square red 16.0
        ]
