module Pong.Game exposing
    ( State(..)
    , WinningScore(..)
    , initialState
    , initialWinningScore
    , winningScoreToInt
    , winningScoreToString
    )

-- MODEL


type State
    = StartingScreen
    | PlayingScreen
    | EndingScreen


type WinningScore
    = Eleven
    | Fifteen



-- INIT


initialState : State
initialState =
    StartingScreen


initialWinningScore : WinningScore
initialWinningScore =
    Eleven



-- HELPERS


winningScoreToInt : WinningScore -> Int
winningScoreToInt winningScore =
    case winningScore of
        Eleven ->
            11

        Fifteen ->
            15


winningScoreToString : WinningScore -> String
winningScoreToString winningScore =
    case winningScore of
        Eleven ->
            "11"

        Fifteen ->
            "15"
