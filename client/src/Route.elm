module Route exposing (Route(..))

-- IMPORTS

import Breakout
import Pong



-- ROUTING


type Route
    = Breakout Breakout.Model
    | Landing
    | Pong Pong.Model
    | NotFound
