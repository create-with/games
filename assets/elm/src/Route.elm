module Route exposing (Route(..))

-- IMPORTS

import Adventure
import Breakout
import Pong
import RandomWalk



-- ROUTING


type Route
    = Adventure Adventure.Model
    | Breakout Breakout.Model
    | Landing
    | Pong Pong.Model
    | RandomWalk RandomWalk.Model
    | NotFound
