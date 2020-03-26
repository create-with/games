module Pong.Game exposing
    ( State(..)
    , Winner
    , WinningScore(..)
    , initialState
    , initialWinner
    , initialWinningScore
    , winningScoreToInt
    , winningScoreToString
    )

-- IMPORTS

import Pong.Paddle



-- MODEL


type State
    = StartingScreen
    | PlayingScreen
    | EndingScreen


type alias Winner =
    Maybe Pong.Paddle.Paddle


type WinningScore
    = Eleven
    | Fifteen



-- INIT


initialState : State
initialState =
    StartingScreen


initialWinner : Winner
initialWinner =
    Nothing


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
