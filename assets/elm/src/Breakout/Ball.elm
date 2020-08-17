module Breakout.Ball exposing
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
import Util.Vector exposing (Vector)



-- MODEL


type alias Ball =
    { position : Vector
    , velocity : Vector
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
    { position = ( 250.0, 250.0 )
    , velocity = ( 400.0, 400.0 )
    , width = 16.0
    , height = 16.0
    }


initialBallPath : BallPath
initialBallPath =
    []


initialShowBallPath : ShowBallPath
initialShowBallPath =
    On



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
    Svg.image
        [ Svg.Attributes.xlinkHref "/images/pixel-ball.png"
        , Svg.Attributes.x <| String.fromFloat <| Util.Vector.getX ball.position
        , Svg.Attributes.y <| String.fromFloat <| Util.Vector.getY ball.position
        , Svg.Attributes.width <| String.fromFloat ball.width
        , Svg.Attributes.height <| String.fromFloat ball.height
        ]
        []


viewBallPath : BallPath -> List (Svg msg)
viewBallPath ballPath =
    List.indexedMap viewBallPathSegment ballPath


viewBallPathSegment : Int -> Ball -> Svg msg
viewBallPathSegment index ball =
    Svg.rect
        [ Svg.Attributes.fillOpacity <| String.fromFloat <| 0.01 * toFloat (25 - index)
        , Svg.Attributes.fill <| "lightyellow"
        , Svg.Attributes.x <| String.fromFloat <| Util.Vector.getX ball.position
        , Svg.Attributes.y <| String.fromFloat <| Util.Vector.getY ball.position
        , Svg.Attributes.width <| String.fromFloat ball.width
        , Svg.Attributes.height <| String.fromFloat ball.height
        ]
        []
