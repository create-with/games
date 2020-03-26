module Util exposing (fpsMeter)

-- IMPORTS

import Svg
import Svg.Attributes



-- TYPES


type alias DeltaTime =
    Float



-- SVG


fpsMeter : List DeltaTime -> Svg.Svg msg
fpsMeter deltaTimes =
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
    Svg.text_
        [ Svg.Attributes.x <| String.fromInt 5
        , Svg.Attributes.y <| String.fromInt 20
        , Svg.Attributes.fill "white"
        , Svg.Attributes.fontFamily "monospace"
        , Svg.Attributes.fontWeight "bold"
        , Svg.Attributes.fontSize "20"
        ]
        [ Svg.text <| fps ++ "fps" ]
