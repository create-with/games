module Breakout.Vector exposing
    ( Vector
    , add
    , negate
    , scale
    , subtract
    )


type alias Vector =
    ( Float, Float )


negate : Vector -> Vector
negate ( x, y ) =
    ( Basics.negate x
    , Basics.negate y
    )


scale : Float -> Vector -> Vector
scale scaleFactor ( x, y ) =
    ( scaleFactor * x
    , scaleFactor * y
    )


add : Vector -> Vector -> Vector
add ( x, y ) ( a, b ) =
    ( x + a
    , y + b
    )


subtract : Vector -> Vector -> Vector
subtract ( x, y ) ( a, b ) =
    ( x - a
    , y - b
    )
