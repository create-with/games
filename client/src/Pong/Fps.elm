module Pong.Fps exposing
    ( DeltaTimes
    , ShowFps(..)
    , initialDeltaTimes
    , initialShowFps
    , showFpsToString
    , viewFps
    )

-- IMPORTS

import Svg
import Svg.Attributes



-- MODEL


type alias DeltaTimes =
    List Float


type ShowFps
    = Off
    | On



-- INIT


initialDeltaTimes : DeltaTimes
initialDeltaTimes =
    []


initialShowFps : ShowFps
initialShowFps =
    Off



-- VIEW


viewFps : ShowFps -> DeltaTimes -> Svg.Svg msg
viewFps showFps deltaTimes =
    let
        average currentWeight sumOfWeights weightedSum list =
            case list of
                [] ->
                    weightedSum / sumOfWeights

                head :: tail ->
                    average
                        (currentWeight * 0.9)
                        (currentWeight + sumOfWeights)
                        (head * currentWeight + weightedSum)
                        tail

        fps =
            String.fromInt <| round <| 1 / average 1 0 0 deltaTimes
    in
    case showFps of
        Off ->
            Svg.g [] []

        On ->
            Svg.text_
                [ Svg.Attributes.x <| String.fromInt 5
                , Svg.Attributes.y <| String.fromInt 20
                , Svg.Attributes.fill "white"
                , Svg.Attributes.fontFamily "monospace"
                , Svg.Attributes.fontWeight "bold"
                , Svg.Attributes.fontSize "20"
                ]
                [ Svg.text <| fps ++ "fps" ]



-- HELPERS


showFpsToString : ShowFps -> String
showFpsToString showFps =
    case showFps of
        On ->
            "On"

        Off ->
            "Off"
