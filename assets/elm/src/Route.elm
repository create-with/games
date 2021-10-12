module Route exposing (Route(..))

-- IMPORTS

import Adventure
import Breakout
import Mario
import Pong



-- ROUTING


type Route
    = Adventure Adventure.Model
    | Breakout Breakout.Model
    | Landing
    | Mario Mario.Model
    | Pong Pong.Model
    | NotFound
