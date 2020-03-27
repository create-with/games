module Pong.Game exposing
    ( DeltaTime
    , State(..)
    , Winner
    , WinningScore(..)
    , getWinner
    , initialState
    , initialWinner
    , initialWinningScore
    , winningScoreToInt
    , winningScoreToString
    )

-- IMPORTS

import Pong.Paddle



-- MODEL


type alias DeltaTime =
    Float


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



-- GET


getWinner : Pong.Paddle.Paddle -> Pong.Paddle.Paddle -> WinningScore -> Maybe Pong.Paddle.Paddle
getWinner leftPaddle rightPaddle winningScore =
    if leftPaddle.score == winningScoreToInt winningScore then
        Just leftPaddle

    else if rightPaddle.score == winningScoreToInt winningScore then
        Just rightPaddle

    else
        Nothing



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
