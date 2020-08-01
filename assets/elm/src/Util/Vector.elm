module Util.Vector exposing
    ( Vector
    , add
    , getX
    , getY
    , negate
    , scale
    , subtract
    )


type alias Vector =
    ( Float, Float )



-- ACCESS


getX : Vector -> Float
getX ( x, _ ) =
    x


getY : Vector -> Float
getY ( _, y ) =
    y



-- TRANSFORM


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
