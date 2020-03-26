module Pong.Paddle exposing
    ( Paddle
    , PaddleId(..)
    , initialLeftPaddle
    , initialRightPaddle
    , paddleIdToString
    )

-- MODEL


type alias Paddle =
    { color : String
    , id : PaddleId
    , x : Int
    , y : Int
    , width : Int
    , height : Int
    }


type PaddleId
    = Left
    | Right



-- INIT


initialLeftPaddle : Paddle
initialLeftPaddle =
    { color = "lightblue"
    , id = Left
    , x = 48
    , y = 200
    , width = 10
    , height = 60
    }


initialRightPaddle : Paddle
initialRightPaddle =
    { color = "lightpink"
    , id = Right
    , x = 740
    , y = 300
    , width = 10
    , height = 60
    }



-- HELPERS


paddleIdToString : PaddleId -> String
paddleIdToString paddleId =
    case paddleId of
        Left ->
            "Left"

        Right ->
            "Right"
