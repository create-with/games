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

import Svg
import Svg.Attributes

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
    = Off
    | On



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

-- VIEW


viewBall : Ball -> Svg.Svg msg
viewBall ball =
    Svg.rect
        [ Svg.Attributes.fill ball.color
        , Svg.Attributes.x <| String.fromInt ball.x
        , Svg.Attributes.y <| String.fromInt ball.y
        , Svg.Attributes.width <| String.fromInt ball.width
        , Svg.Attributes.height <| String.fromInt ball.height
        ]
        []


viewBallPath : ShowBallPath -> List Ball -> List (Svg.Svg msg)
viewBallPath showBallPath ballPath =
    case showBallPath of
        On ->
            List.indexedMap viewBallPathSegment ballPath

        Off ->
            []


viewBallPathSegment : Int -> Ball -> Svg.Svg msg
viewBallPathSegment index ball =
    Svg.rect
        [ Svg.Attributes.fillOpacity <| String.fromFloat <| 0.01 * toFloat (80 - index)
        , Svg.Attributes.fill "darkorange"
        , Svg.Attributes.x <| String.fromInt ball.x
        , Svg.Attributes.y <| String.fromInt ball.y
        , Svg.Attributes.width <| String.fromInt ball.width
        , Svg.Attributes.height <| String.fromInt ball.height
        ]
        []