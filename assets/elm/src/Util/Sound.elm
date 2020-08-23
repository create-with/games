module Util.Sound exposing
    ( PlayMusic(..)
    , initialPlayMusic
    , playMusicToBool
    , playMusicToString
    , stopMusic
    )

-- IMPORTS

import Json.Encode
import Util.Ports



-- MODEL


type PlayMusic
    = On
    | Off



-- INIT


initialPlayMusic : PlayMusic
initialPlayMusic =
    On



-- HELPERS


playMusicToBool : PlayMusic -> Bool
playMusicToBool playMusic =
    case playMusic of
        On ->
            True

        Off ->
            False


playMusicToString : PlayMusic -> String
playMusicToString playMusic =
    case playMusic of
        On ->
            "On"

        Off ->
            "Off"


stopMusic : Cmd a
stopMusic =
    Util.Ports.playMusic <| Json.Encode.object [ ( "play", Json.Encode.bool False ) ]
