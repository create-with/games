module Breakout.Ball exposing
    ( Ball
    , BallPath
    , initialBall
    , initialBallPath
    , viewBall
    , viewBallPath
    )

-- IMPORTS

import Breakout.Vector exposing (Vector)
import Svg exposing (Svg)
import Svg.Attributes



-- MODEL


type alias Ball =
    { position : Vector
    , velocity : Vector
    , width : Float
    , height : Float
    }


type alias BallPath =
    List Ball



-- INIT


initialBall : Ball
initialBall =
    { position = ( 395.0, 310.0 )
    , velocity = ( 375.0, 375.0 )
    , width = 16.0
    , height = 16.0
    }


initialBallPath : BallPath
initialBallPath =
    []



-- VIEW


viewBall : Ball -> Svg msg
viewBall ball =
    let
        ( x, y ) =
            ball.position
    in
    Svg.image
        [ Svg.Attributes.xlinkHref "/images/pixel-ball.png"
        , Svg.Attributes.x <| String.fromFloat x
        , Svg.Attributes.y <| String.fromFloat y
        , Svg.Attributes.width <| String.fromFloat ball.width
        , Svg.Attributes.height <| String.fromFloat ball.height
        ]
        []


viewBallPath : BallPath -> List (Svg msg)
viewBallPath ballPath =
    List.indexedMap viewBallPathSegment ballPath


viewBallPathSegment : Int -> Ball -> Svg msg
viewBallPathSegment index ball =
    let
        ( x, y ) =
            ball.position
    in
    Svg.rect
        [ Svg.Attributes.fillOpacity <| String.fromFloat <| 0.01 * toFloat (80 - index)
        , Svg.Attributes.fill <| "darkorange"
        , Svg.Attributes.x <| String.fromFloat x
        , Svg.Attributes.y <| String.fromFloat y
        , Svg.Attributes.width <| String.fromFloat ball.width
        , Svg.Attributes.height <| String.fromFloat ball.height
        ]
        []
