module Pong.Paddle exposing
    ( Paddle
    , PaddleId(..)
    , initialLeftPaddle
    , initialRightPaddle
    , paddleIdToString
    , updateScore
    )

-- MODEL


type alias Paddle =
    { color : String
    , id : PaddleId
    , score : Int
    , x : Int
    , y : Int
    , vy : Float
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
    , score = 0
    , x = 48
    , y = 200
    , vy = 300.0
    , width = 10
    , height = 60
    }


initialRightPaddle : Paddle
initialRightPaddle =
    { color = "lightpink"
    , id = Right
    , score = 0
    , x = 740
    , y = 300
    , vy = 300.0
    , width = 10
    , height = 60
    }



-- UPDATE


updateScore : Paddle -> Paddle
updateScore paddle =
    { paddle | score = paddle.score + 1 }



-- HELPERS


paddleIdToString : PaddleId -> String
paddleIdToString paddleId =
    case paddleId of
        Left ->
            "Left"

        Right ->
            "Right"
