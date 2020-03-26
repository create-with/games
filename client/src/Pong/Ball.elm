module Pong.Ball exposing
    ( Ball
    , BallPath
    , ShowBallPath(..)
    , initialBall
    , initialBallPath
    , initialShowBallPath
    , showBallPathToString
    )

-- MODEL


type alias Ball =
    { color : String
    , x : Int
    , y : Int
    , vx : Float
    , vy : Float
    , width : Int
    , height : Int
    }


type alias BallPath =
    List Ball


type ShowBallPath
    = On
    | Off



-- INIT


initialBall : Ball
initialBall =
    { color = "white"
    , x = 400
    , y = 300
    , vx = 300.0
    , vy = 300.0
    , width = 10
    , height = 10
    }


initialBallPath : BallPath
initialBallPath =
    []


initialShowBallPath : ShowBallPath
initialShowBallPath =
    Off



-- HELPERS


showBallPathToString : ShowBallPath -> String
showBallPathToString showBallPath =
    case showBallPath of
        On ->
            "On"

        Off ->
            "Off"
