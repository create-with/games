port module Util.Ports exposing
    ( playMusic
    , playSound
    )

-- IMPORTS

import Json.Encode exposing (Value)



-- PORTS


port playMusic : Value -> Cmd msg


port playSound : Value -> Cmd msg
