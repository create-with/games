module Route exposing (Route(..))

-- IMPORTS

import Adventure
import Breakout
import Pong



-- ROUTING


type Route
    = Adventure Adventure.Model
    | Breakout Breakout.Model
    | Landing
    | Pong Pong.Model
    | NotFound
