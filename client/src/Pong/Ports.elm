port module Pong.Ports exposing (playSound)

-- IMPORTS

import Json.Encode exposing (Value)



-- PORTS


port playSound : Value -> Cmd msg
