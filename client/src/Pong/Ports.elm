port module Pong.Ports exposing (playSound)

-- IMPORTS

import Json.Encode



-- PORTS


port playSound : Json.Encode.Value -> Cmd msg
