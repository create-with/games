module Util.Sound exposing
    ( PlayMusic(..)
    , initialPlayMusic
    , playMusicToBool
    , playMusicToString
    )

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
