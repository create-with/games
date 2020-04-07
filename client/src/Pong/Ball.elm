module Pong.Ball exposing
    ( Ball
    , BallPath
    , ShowBallPath(..)
    , initialBall
    , initialBallPath
    , initialShowBallPath
    , showBallPathToString
    , viewBall
    , viewBallPath
    )

-- IMPORTS

import Svg exposing (Svg)
import Svg.Attributes



-- MODEL


type alias Ball =
    { color : String
    , x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , width : Float
    , height : Float
    }


type alias BallPath =
    List Ball


type ShowBallPath
    = Off
    | On



-- INIT


initialBall : Ball
initialBall =
    { color = "white"
    , x = 395.0
    , y = 310.0
    , vx = 350.0
    , vy = 350.0
    , width = 10.0
    , height = 10.0
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



-- VIEW


viewBall : Ball -> Svg msg
viewBall ball =
    Svg.rect
        [ Svg.Attributes.fill <| ball.color
        , Svg.Attributes.x <| String.fromFloat ball.x
        , Svg.Attributes.y <| String.fromFloat ball.y
        , Svg.Attributes.width <| String.fromFloat ball.width
        , Svg.Attributes.height <| String.fromFloat ball.height
        ]
        []


viewBallPath : ShowBallPath -> BallPath -> List (Svg msg)
viewBallPath showBallPath ballPath =
    case showBallPath of
        On ->
            List.indexedMap viewBallPathSegment ballPath

        Off ->
            []


viewBallPathSegment : Int -> Ball -> Svg msg
viewBallPathSegment index ball =
    Svg.rect
        [ Svg.Attributes.fillOpacity <| String.fromFloat <| 0.01 * toFloat (80 - index)
        , Svg.Attributes.fill <| "darkorange"
        , Svg.Attributes.x <| String.fromFloat ball.x
        , Svg.Attributes.y <| String.fromFloat ball.y
        , Svg.Attributes.width <| String.fromFloat ball.width
        , Svg.Attributes.height <| String.fromFloat ball.height
        ]
        []
