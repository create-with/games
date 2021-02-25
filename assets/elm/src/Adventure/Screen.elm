module Adventure.Screen exposing
    ( Block
    , Screen
    , screen01
    , view
    )

-- IMPORTS

import Svg exposing (Svg)
import Svg.Attributes



-- MODEL


type alias Block =
    { color : String
    , x : Float
    , y : Float
    , width : Float
    , height : Float
    }


type alias BlockLocation =
    ( Int, Int )


type alias Screen =
    List BlockLocation



{- See assets/elm/static/screens for raw map file. -}


screen01 : Screen
screen01 =
    [ ( 0, 0 )
    , ( 1, 0 )
    , ( 2, 0 )
    , ( 3, 0 )
    , ( 4, 0 )
    , ( 5, 0 )
    , ( 6, 0 )
    , ( 7, 0 )
    , ( 8, 0 )
    , ( 9, 0 )
    , ( 10, 0 )
    , ( 11, 0 )
    , ( 12, 0 )
    , ( 13, 0 )
    , ( 14, 0 )
    , ( 15, 0 )
    , ( 16, 0 )
    , ( 17, 0 )
    , ( 18, 0 )
    , ( 19, 0 )
    , ( 20, 0 )
    , ( 21, 0 )
    , ( 24, 0 )
    , ( 25, 0 )
    , ( 28, 0 )
    , ( 29, 0 )
    , ( 32, 0 )
    , ( 33, 0 )
    , ( 46, 0 )
    , ( 47, 0 )
    , ( 50, 0 )
    , ( 51, 0 )
    , ( 54, 0 )
    , ( 55, 0 )
    , ( 58, 0 )
    , ( 59, 0 )
    , ( 60, 0 )
    , ( 61, 0 )
    , ( 62, 0 )
    , ( 63, 0 )
    , ( 64, 0 )
    , ( 65, 0 )
    , ( 66, 0 )
    , ( 67, 0 )
    , ( 68, 0 )
    , ( 69, 0 )
    , ( 70, 0 )
    , ( 71, 0 )
    , ( 72, 0 )
    , ( 73, 0 )
    , ( 74, 0 )
    , ( 75, 0 )
    , ( 76, 0 )
    , ( 77, 0 )
    , ( 78, 0 )
    , ( 79, 0 )
    , ( 0, 1 )
    , ( 1, 1 )
    , ( 2, 1 )
    , ( 3, 1 )
    , ( 4, 1 )
    , ( 5, 1 )
    , ( 6, 1 )
    , ( 7, 1 )
    , ( 8, 1 )
    , ( 9, 1 )
    , ( 10, 1 )
    , ( 11, 1 )
    , ( 12, 1 )
    , ( 13, 1 )
    , ( 14, 1 )
    , ( 15, 1 )
    , ( 16, 1 )
    , ( 17, 1 )
    , ( 18, 1 )
    , ( 19, 1 )
    , ( 20, 1 )
    , ( 21, 1 )
    , ( 24, 1 )
    , ( 25, 1 )
    , ( 28, 1 )
    , ( 29, 1 )
    , ( 32, 1 )
    , ( 33, 1 )
    , ( 46, 1 )
    , ( 47, 1 )
    , ( 50, 1 )
    , ( 51, 1 )
    , ( 54, 1 )
    , ( 55, 1 )
    , ( 58, 1 )
    , ( 59, 1 )
    , ( 60, 1 )
    , ( 61, 1 )
    , ( 62, 1 )
    , ( 63, 1 )
    , ( 64, 1 )
    , ( 65, 1 )
    , ( 66, 1 )
    , ( 67, 1 )
    , ( 68, 1 )
    , ( 69, 1 )
    , ( 70, 1 )
    , ( 71, 1 )
    , ( 72, 1 )
    , ( 73, 1 )
    , ( 74, 1 )
    , ( 75, 1 )
    , ( 76, 1 )
    , ( 77, 1 )
    , ( 78, 1 )
    , ( 79, 1 )
    , ( 0, 2 )
    , ( 1, 2 )
    , ( 2, 2 )
    , ( 3, 2 )
    , ( 4, 2 )
    , ( 5, 2 )
    , ( 6, 2 )
    , ( 7, 2 )
    , ( 8, 2 )
    , ( 9, 2 )
    , ( 10, 2 )
    , ( 11, 2 )
    , ( 12, 2 )
    , ( 13, 2 )
    , ( 14, 2 )
    , ( 15, 2 )
    , ( 16, 2 )
    , ( 17, 2 )
    , ( 18, 2 )
    , ( 19, 2 )
    , ( 20, 2 )
    , ( 21, 2 )
    , ( 24, 2 )
    , ( 25, 2 )
    , ( 28, 2 )
    , ( 29, 2 )
    , ( 32, 2 )
    , ( 33, 2 )
    , ( 46, 2 )
    , ( 47, 2 )
    , ( 50, 2 )
    , ( 51, 2 )
    , ( 54, 2 )
    , ( 55, 2 )
    , ( 58, 2 )
    , ( 59, 2 )
    , ( 60, 2 )
    , ( 61, 2 )
    , ( 62, 2 )
    , ( 63, 2 )
    , ( 64, 2 )
    , ( 65, 2 )
    , ( 66, 2 )
    , ( 67, 2 )
    , ( 68, 2 )
    , ( 69, 2 )
    , ( 70, 2 )
    , ( 71, 2 )
    , ( 72, 2 )
    , ( 73, 2 )
    , ( 74, 2 )
    , ( 75, 2 )
    , ( 76, 2 )
    , ( 77, 2 )
    , ( 78, 2 )
    , ( 79, 2 )
    , ( 0, 3 )
    , ( 1, 3 )
    , ( 2, 3 )
    , ( 3, 3 )
    , ( 4, 3 )
    , ( 5, 3 )
    , ( 6, 3 )
    , ( 7, 3 )
    , ( 8, 3 )
    , ( 9, 3 )
    , ( 10, 3 )
    , ( 11, 3 )
    , ( 12, 3 )
    , ( 13, 3 )
    , ( 14, 3 )
    , ( 15, 3 )
    , ( 16, 3 )
    , ( 17, 3 )
    , ( 18, 3 )
    , ( 19, 3 )
    , ( 20, 3 )
    , ( 21, 3 )
    , ( 24, 3 )
    , ( 25, 3 )
    , ( 28, 3 )
    , ( 29, 3 )
    , ( 32, 3 )
    , ( 33, 3 )
    , ( 46, 3 )
    , ( 47, 3 )
    , ( 50, 3 )
    , ( 51, 3 )
    , ( 54, 3 )
    , ( 55, 3 )
    , ( 58, 3 )
    , ( 59, 3 )
    , ( 60, 3 )
    , ( 61, 3 )
    , ( 62, 3 )
    , ( 63, 3 )
    , ( 64, 3 )
    , ( 65, 3 )
    , ( 66, 3 )
    , ( 67, 3 )
    , ( 68, 3 )
    , ( 69, 3 )
    , ( 70, 3 )
    , ( 71, 3 )
    , ( 72, 3 )
    , ( 73, 3 )
    , ( 74, 3 )
    , ( 75, 3 )
    , ( 76, 3 )
    , ( 77, 3 )
    , ( 78, 3 )
    , ( 79, 3 )
    , ( 0, 4 )
    , ( 1, 4 )
    , ( 2, 4 )
    , ( 3, 4 )
    , ( 4, 4 )
    , ( 5, 4 )
    , ( 6, 4 )
    , ( 7, 4 )
    , ( 8, 4 )
    , ( 9, 4 )
    , ( 10, 4 )
    , ( 11, 4 )
    , ( 12, 4 )
    , ( 13, 4 )
    , ( 14, 4 )
    , ( 15, 4 )
    , ( 16, 4 )
    , ( 17, 4 )
    , ( 18, 4 )
    , ( 19, 4 )
    , ( 20, 4 )
    , ( 21, 4 )
    , ( 24, 4 )
    , ( 25, 4 )
    , ( 28, 4 )
    , ( 29, 4 )
    , ( 32, 4 )
    , ( 33, 4 )
    , ( 46, 4 )
    , ( 47, 4 )
    , ( 50, 4 )
    , ( 51, 4 )
    , ( 54, 4 )
    , ( 55, 4 )
    , ( 58, 4 )
    , ( 59, 4 )
    , ( 60, 4 )
    , ( 61, 4 )
    , ( 62, 4 )
    , ( 63, 4 )
    , ( 64, 4 )
    , ( 65, 4 )
    , ( 66, 4 )
    , ( 67, 4 )
    , ( 68, 4 )
    , ( 69, 4 )
    , ( 70, 4 )
    , ( 71, 4 )
    , ( 72, 4 )
    , ( 73, 4 )
    , ( 74, 4 )
    , ( 75, 4 )
    , ( 76, 4 )
    , ( 77, 4 )
    , ( 78, 4 )
    , ( 79, 4 )
    , ( 0, 5 )
    , ( 1, 5 )
    , ( 2, 5 )
    , ( 3, 5 )
    , ( 20, 5 )
    , ( 21, 5 )
    , ( 22, 5 )
    , ( 23, 5 )
    , ( 24, 5 )
    , ( 25, 5 )
    , ( 26, 5 )
    , ( 27, 5 )
    , ( 28, 5 )
    , ( 29, 5 )
    , ( 30, 5 )
    , ( 31, 5 )
    , ( 32, 5 )
    , ( 33, 5 )
    , ( 46, 5 )
    , ( 47, 5 )
    , ( 48, 5 )
    , ( 49, 5 )
    , ( 50, 5 )
    , ( 51, 5 )
    , ( 52, 5 )
    , ( 53, 5 )
    , ( 54, 5 )
    , ( 55, 5 )
    , ( 56, 5 )
    , ( 57, 5 )
    , ( 58, 5 )
    , ( 59, 5 )
    , ( 76, 5 )
    , ( 77, 5 )
    , ( 78, 5 )
    , ( 79, 5 )
    , ( 0, 6 )
    , ( 1, 6 )
    , ( 2, 6 )
    , ( 3, 6 )
    , ( 20, 6 )
    , ( 21, 6 )
    , ( 22, 6 )
    , ( 23, 6 )
    , ( 24, 6 )
    , ( 25, 6 )
    , ( 26, 6 )
    , ( 27, 6 )
    , ( 28, 6 )
    , ( 29, 6 )
    , ( 30, 6 )
    , ( 31, 6 )
    , ( 32, 6 )
    , ( 33, 6 )
    , ( 46, 6 )
    , ( 47, 6 )
    , ( 48, 6 )
    , ( 49, 6 )
    , ( 50, 6 )
    , ( 51, 6 )
    , ( 52, 6 )
    , ( 53, 6 )
    , ( 54, 6 )
    , ( 55, 6 )
    , ( 56, 6 )
    , ( 57, 6 )
    , ( 58, 6 )
    , ( 59, 6 )
    , ( 76, 6 )
    , ( 77, 6 )
    , ( 78, 6 )
    , ( 79, 6 )
    , ( 0, 7 )
    , ( 1, 7 )
    , ( 2, 7 )
    , ( 3, 7 )
    , ( 20, 7 )
    , ( 21, 7 )
    , ( 22, 7 )
    , ( 23, 7 )
    , ( 24, 7 )
    , ( 25, 7 )
    , ( 26, 7 )
    , ( 27, 7 )
    , ( 28, 7 )
    , ( 29, 7 )
    , ( 30, 7 )
    , ( 31, 7 )
    , ( 32, 7 )
    , ( 33, 7 )
    , ( 46, 7 )
    , ( 47, 7 )
    , ( 48, 7 )
    , ( 49, 7 )
    , ( 50, 7 )
    , ( 51, 7 )
    , ( 52, 7 )
    , ( 53, 7 )
    , ( 54, 7 )
    , ( 55, 7 )
    , ( 56, 7 )
    , ( 57, 7 )
    , ( 58, 7 )
    , ( 59, 7 )
    , ( 76, 7 )
    , ( 77, 7 )
    , ( 78, 7 )
    , ( 79, 7 )
    , ( 0, 8 )
    , ( 1, 8 )
    , ( 2, 8 )
    , ( 3, 8 )
    , ( 20, 8 )
    , ( 21, 8 )
    , ( 22, 8 )
    , ( 23, 8 )
    , ( 24, 8 )
    , ( 25, 8 )
    , ( 26, 8 )
    , ( 27, 8 )
    , ( 28, 8 )
    , ( 29, 8 )
    , ( 30, 8 )
    , ( 31, 8 )
    , ( 32, 8 )
    , ( 33, 8 )
    , ( 46, 8 )
    , ( 47, 8 )
    , ( 48, 8 )
    , ( 49, 8 )
    , ( 50, 8 )
    , ( 51, 8 )
    , ( 52, 8 )
    , ( 53, 8 )
    , ( 54, 8 )
    , ( 55, 8 )
    , ( 56, 8 )
    , ( 57, 8 )
    , ( 58, 8 )
    , ( 59, 8 )
    , ( 76, 8 )
    , ( 77, 8 )
    , ( 78, 8 )
    , ( 79, 8 )
    , ( 0, 9 )
    , ( 1, 9 )
    , ( 2, 9 )
    , ( 3, 9 )
    , ( 20, 9 )
    , ( 21, 9 )
    , ( 22, 9 )
    , ( 23, 9 )
    , ( 24, 9 )
    , ( 25, 9 )
    , ( 26, 9 )
    , ( 27, 9 )
    , ( 28, 9 )
    , ( 29, 9 )
    , ( 30, 9 )
    , ( 31, 9 )
    , ( 32, 9 )
    , ( 33, 9 )
    , ( 46, 9 )
    , ( 47, 9 )
    , ( 48, 9 )
    , ( 49, 9 )
    , ( 50, 9 )
    , ( 51, 9 )
    , ( 52, 9 )
    , ( 53, 9 )
    , ( 54, 9 )
    , ( 55, 9 )
    , ( 56, 9 )
    , ( 57, 9 )
    , ( 58, 9 )
    , ( 59, 9 )
    , ( 76, 9 )
    , ( 77, 9 )
    , ( 78, 9 )
    , ( 79, 9 )
    , ( 0, 10 )
    , ( 1, 10 )
    , ( 2, 10 )
    , ( 3, 10 )
    , ( 20, 10 )
    , ( 21, 10 )
    , ( 22, 10 )
    , ( 23, 10 )
    , ( 24, 10 )
    , ( 25, 10 )
    , ( 26, 10 )
    , ( 27, 10 )
    , ( 28, 10 )
    , ( 29, 10 )
    , ( 30, 10 )
    , ( 31, 10 )
    , ( 32, 10 )
    , ( 33, 10 )
    , ( 46, 10 )
    , ( 47, 10 )
    , ( 48, 10 )
    , ( 49, 10 )
    , ( 50, 10 )
    , ( 51, 10 )
    , ( 52, 10 )
    , ( 53, 10 )
    , ( 54, 10 )
    , ( 55, 10 )
    , ( 56, 10 )
    , ( 57, 10 )
    , ( 58, 10 )
    , ( 59, 10 )
    , ( 76, 10 )
    , ( 77, 10 )
    , ( 78, 10 )
    , ( 79, 10 )
    , ( 0, 11 )
    , ( 1, 11 )
    , ( 2, 11 )
    , ( 3, 11 )
    , ( 20, 11 )
    , ( 21, 11 )
    , ( 22, 11 )
    , ( 23, 11 )
    , ( 24, 11 )
    , ( 25, 11 )
    , ( 26, 11 )
    , ( 27, 11 )
    , ( 28, 11 )
    , ( 29, 11 )
    , ( 30, 11 )
    , ( 31, 11 )
    , ( 32, 11 )
    , ( 33, 11 )
    , ( 46, 11 )
    , ( 47, 11 )
    , ( 48, 11 )
    , ( 49, 11 )
    , ( 50, 11 )
    , ( 51, 11 )
    , ( 52, 11 )
    , ( 53, 11 )
    , ( 54, 11 )
    , ( 55, 11 )
    , ( 56, 11 )
    , ( 57, 11 )
    , ( 58, 11 )
    , ( 59, 11 )
    , ( 76, 11 )
    , ( 77, 11 )
    , ( 78, 11 )
    , ( 79, 11 )
    , ( 0, 12 )
    , ( 1, 12 )
    , ( 2, 12 )
    , ( 3, 12 )
    , ( 20, 12 )
    , ( 21, 12 )
    , ( 22, 12 )
    , ( 23, 12 )
    , ( 24, 12 )
    , ( 25, 12 )
    , ( 26, 12 )
    , ( 27, 12 )
    , ( 28, 12 )
    , ( 29, 12 )
    , ( 30, 12 )
    , ( 31, 12 )
    , ( 32, 12 )
    , ( 33, 12 )
    , ( 46, 12 )
    , ( 47, 12 )
    , ( 48, 12 )
    , ( 49, 12 )
    , ( 50, 12 )
    , ( 51, 12 )
    , ( 52, 12 )
    , ( 53, 12 )
    , ( 54, 12 )
    , ( 55, 12 )
    , ( 56, 12 )
    , ( 57, 12 )
    , ( 58, 12 )
    , ( 59, 12 )
    , ( 76, 12 )
    , ( 77, 12 )
    , ( 78, 12 )
    , ( 79, 12 )
    , ( 0, 13 )
    , ( 1, 13 )
    , ( 2, 13 )
    , ( 3, 13 )
    , ( 20, 13 )
    , ( 21, 13 )
    , ( 22, 13 )
    , ( 23, 13 )
    , ( 24, 13 )
    , ( 25, 13 )
    , ( 26, 13 )
    , ( 27, 13 )
    , ( 28, 13 )
    , ( 29, 13 )
    , ( 30, 13 )
    , ( 31, 13 )
    , ( 32, 13 )
    , ( 33, 13 )
    , ( 46, 13 )
    , ( 47, 13 )
    , ( 48, 13 )
    , ( 49, 13 )
    , ( 50, 13 )
    , ( 51, 13 )
    , ( 52, 13 )
    , ( 53, 13 )
    , ( 54, 13 )
    , ( 55, 13 )
    , ( 56, 13 )
    , ( 57, 13 )
    , ( 58, 13 )
    , ( 59, 13 )
    , ( 76, 13 )
    , ( 77, 13 )
    , ( 78, 13 )
    , ( 79, 13 )
    , ( 0, 14 )
    , ( 1, 14 )
    , ( 2, 14 )
    , ( 3, 14 )
    , ( 20, 14 )
    , ( 21, 14 )
    , ( 22, 14 )
    , ( 23, 14 )
    , ( 24, 14 )
    , ( 25, 14 )
    , ( 26, 14 )
    , ( 27, 14 )
    , ( 28, 14 )
    , ( 29, 14 )
    , ( 30, 14 )
    , ( 31, 14 )
    , ( 32, 14 )
    , ( 33, 14 )
    , ( 34, 14 )
    , ( 35, 14 )
    , ( 36, 14 )
    , ( 37, 14 )
    , ( 38, 14 )
    , ( 39, 14 )
    , ( 40, 14 )
    , ( 41, 14 )
    , ( 42, 14 )
    , ( 43, 14 )
    , ( 44, 14 )
    , ( 45, 14 )
    , ( 46, 14 )
    , ( 47, 14 )
    , ( 48, 14 )
    , ( 49, 14 )
    , ( 50, 14 )
    , ( 51, 14 )
    , ( 52, 14 )
    , ( 53, 14 )
    , ( 54, 14 )
    , ( 55, 14 )
    , ( 56, 14 )
    , ( 57, 14 )
    , ( 58, 14 )
    , ( 59, 14 )
    , ( 76, 14 )
    , ( 77, 14 )
    , ( 78, 14 )
    , ( 79, 14 )
    , ( 0, 15 )
    , ( 1, 15 )
    , ( 2, 15 )
    , ( 3, 15 )
    , ( 20, 15 )
    , ( 21, 15 )
    , ( 22, 15 )
    , ( 23, 15 )
    , ( 24, 15 )
    , ( 25, 15 )
    , ( 26, 15 )
    , ( 27, 15 )
    , ( 28, 15 )
    , ( 29, 15 )
    , ( 30, 15 )
    , ( 31, 15 )
    , ( 32, 15 )
    , ( 33, 15 )
    , ( 34, 15 )
    , ( 35, 15 )
    , ( 36, 15 )
    , ( 37, 15 )
    , ( 38, 15 )
    , ( 39, 15 )
    , ( 40, 15 )
    , ( 41, 15 )
    , ( 42, 15 )
    , ( 43, 15 )
    , ( 44, 15 )
    , ( 45, 15 )
    , ( 46, 15 )
    , ( 47, 15 )
    , ( 48, 15 )
    , ( 49, 15 )
    , ( 50, 15 )
    , ( 51, 15 )
    , ( 52, 15 )
    , ( 53, 15 )
    , ( 54, 15 )
    , ( 55, 15 )
    , ( 56, 15 )
    , ( 57, 15 )
    , ( 58, 15 )
    , ( 59, 15 )
    , ( 76, 15 )
    , ( 77, 15 )
    , ( 78, 15 )
    , ( 79, 15 )
    , ( 0, 16 )
    , ( 1, 16 )
    , ( 2, 16 )
    , ( 3, 16 )
    , ( 20, 16 )
    , ( 21, 16 )
    , ( 22, 16 )
    , ( 23, 16 )
    , ( 24, 16 )
    , ( 25, 16 )
    , ( 26, 16 )
    , ( 27, 16 )
    , ( 28, 16 )
    , ( 29, 16 )
    , ( 30, 16 )
    , ( 31, 16 )
    , ( 32, 16 )
    , ( 33, 16 )
    , ( 34, 16 )
    , ( 35, 16 )
    , ( 36, 16 )
    , ( 37, 16 )
    , ( 38, 16 )
    , ( 39, 16 )
    , ( 40, 16 )
    , ( 41, 16 )
    , ( 42, 16 )
    , ( 43, 16 )
    , ( 44, 16 )
    , ( 45, 16 )
    , ( 46, 16 )
    , ( 47, 16 )
    , ( 48, 16 )
    , ( 49, 16 )
    , ( 50, 16 )
    , ( 51, 16 )
    , ( 52, 16 )
    , ( 53, 16 )
    , ( 54, 16 )
    , ( 55, 16 )
    , ( 56, 16 )
    , ( 57, 16 )
    , ( 58, 16 )
    , ( 59, 16 )
    , ( 76, 16 )
    , ( 77, 16 )
    , ( 78, 16 )
    , ( 79, 16 )
    , ( 0, 17 )
    , ( 1, 17 )
    , ( 2, 17 )
    , ( 3, 17 )
    , ( 20, 17 )
    , ( 21, 17 )
    , ( 22, 17 )
    , ( 23, 17 )
    , ( 24, 17 )
    , ( 25, 17 )
    , ( 26, 17 )
    , ( 27, 17 )
    , ( 28, 17 )
    , ( 29, 17 )
    , ( 30, 17 )
    , ( 31, 17 )
    , ( 32, 17 )
    , ( 33, 17 )
    , ( 34, 17 )
    , ( 35, 17 )
    , ( 36, 17 )
    , ( 37, 17 )
    , ( 38, 17 )
    , ( 39, 17 )
    , ( 40, 17 )
    , ( 41, 17 )
    , ( 42, 17 )
    , ( 43, 17 )
    , ( 44, 17 )
    , ( 45, 17 )
    , ( 46, 17 )
    , ( 47, 17 )
    , ( 48, 17 )
    , ( 49, 17 )
    , ( 50, 17 )
    , ( 51, 17 )
    , ( 52, 17 )
    , ( 53, 17 )
    , ( 54, 17 )
    , ( 55, 17 )
    , ( 56, 17 )
    , ( 57, 17 )
    , ( 58, 17 )
    , ( 59, 17 )
    , ( 76, 17 )
    , ( 77, 17 )
    , ( 78, 17 )
    , ( 79, 17 )
    , ( 0, 18 )
    , ( 1, 18 )
    , ( 2, 18 )
    , ( 3, 18 )
    , ( 20, 18 )
    , ( 21, 18 )
    , ( 22, 18 )
    , ( 23, 18 )
    , ( 24, 18 )
    , ( 25, 18 )
    , ( 26, 18 )
    , ( 27, 18 )
    , ( 28, 18 )
    , ( 29, 18 )
    , ( 30, 18 )
    , ( 31, 18 )
    , ( 32, 18 )
    , ( 33, 18 )
    , ( 34, 18 )
    , ( 35, 18 )
    , ( 36, 18 )
    , ( 37, 18 )
    , ( 38, 18 )
    , ( 39, 18 )
    , ( 40, 18 )
    , ( 41, 18 )
    , ( 42, 18 )
    , ( 43, 18 )
    , ( 44, 18 )
    , ( 45, 18 )
    , ( 46, 18 )
    , ( 47, 18 )
    , ( 48, 18 )
    , ( 49, 18 )
    , ( 50, 18 )
    , ( 51, 18 )
    , ( 52, 18 )
    , ( 53, 18 )
    , ( 54, 18 )
    , ( 55, 18 )
    , ( 56, 18 )
    , ( 57, 18 )
    , ( 58, 18 )
    , ( 59, 18 )
    , ( 76, 18 )
    , ( 77, 18 )
    , ( 78, 18 )
    , ( 79, 18 )
    , ( 0, 19 )
    , ( 1, 19 )
    , ( 2, 19 )
    , ( 3, 19 )
    , ( 20, 19 )
    , ( 21, 19 )
    , ( 22, 19 )
    , ( 23, 19 )
    , ( 24, 19 )
    , ( 25, 19 )
    , ( 26, 19 )
    , ( 27, 19 )
    , ( 28, 19 )
    , ( 29, 19 )
    , ( 30, 19 )
    , ( 31, 19 )
    , ( 32, 19 )
    , ( 33, 19 )
    , ( 34, 19 )
    , ( 35, 19 )
    , ( 36, 19 )
    , ( 37, 19 )
    , ( 38, 19 )
    , ( 39, 19 )
    , ( 40, 19 )
    , ( 41, 19 )
    , ( 42, 19 )
    , ( 43, 19 )
    , ( 44, 19 )
    , ( 45, 19 )
    , ( 46, 19 )
    , ( 47, 19 )
    , ( 48, 19 )
    , ( 49, 19 )
    , ( 50, 19 )
    , ( 51, 19 )
    , ( 52, 19 )
    , ( 53, 19 )
    , ( 54, 19 )
    , ( 55, 19 )
    , ( 56, 19 )
    , ( 57, 19 )
    , ( 58, 19 )
    , ( 59, 19 )
    , ( 76, 19 )
    , ( 77, 19 )
    , ( 78, 19 )
    , ( 79, 19 )
    , ( 0, 20 )
    , ( 1, 20 )
    , ( 2, 20 )
    , ( 3, 20 )
    , ( 20, 20 )
    , ( 21, 20 )
    , ( 22, 20 )
    , ( 23, 20 )
    , ( 24, 20 )
    , ( 25, 20 )
    , ( 26, 20 )
    , ( 27, 20 )
    , ( 28, 20 )
    , ( 29, 20 )
    , ( 30, 20 )
    , ( 31, 20 )
    , ( 32, 20 )
    , ( 33, 20 )
    , ( 34, 20 )
    , ( 35, 20 )
    , ( 36, 20 )
    , ( 37, 20 )
    , ( 38, 20 )
    , ( 39, 20 )
    , ( 40, 20 )
    , ( 41, 20 )
    , ( 42, 20 )
    , ( 43, 20 )
    , ( 44, 20 )
    , ( 45, 20 )
    , ( 46, 20 )
    , ( 47, 20 )
    , ( 48, 20 )
    , ( 49, 20 )
    , ( 50, 20 )
    , ( 51, 20 )
    , ( 52, 20 )
    , ( 53, 20 )
    , ( 54, 20 )
    , ( 55, 20 )
    , ( 56, 20 )
    , ( 57, 20 )
    , ( 58, 20 )
    , ( 59, 20 )
    , ( 76, 20 )
    , ( 77, 20 )
    , ( 78, 20 )
    , ( 79, 20 )
    , ( 0, 21 )
    , ( 1, 21 )
    , ( 2, 21 )
    , ( 3, 21 )
    , ( 20, 21 )
    , ( 21, 21 )
    , ( 22, 21 )
    , ( 23, 21 )
    , ( 24, 21 )
    , ( 25, 21 )
    , ( 26, 21 )
    , ( 27, 21 )
    , ( 28, 21 )
    , ( 29, 21 )
    , ( 30, 21 )
    , ( 31, 21 )
    , ( 32, 21 )
    , ( 33, 21 )
    , ( 34, 21 )
    , ( 35, 21 )
    , ( 36, 21 )
    , ( 37, 21 )
    , ( 38, 21 )
    , ( 39, 21 )
    , ( 40, 21 )
    , ( 41, 21 )
    , ( 42, 21 )
    , ( 43, 21 )
    , ( 44, 21 )
    , ( 45, 21 )
    , ( 46, 21 )
    , ( 47, 21 )
    , ( 48, 21 )
    , ( 49, 21 )
    , ( 50, 21 )
    , ( 51, 21 )
    , ( 52, 21 )
    , ( 53, 21 )
    , ( 54, 21 )
    , ( 55, 21 )
    , ( 56, 21 )
    , ( 57, 21 )
    , ( 58, 21 )
    , ( 59, 21 )
    , ( 76, 21 )
    , ( 77, 21 )
    , ( 78, 21 )
    , ( 79, 21 )
    , ( 0, 22 )
    , ( 1, 22 )
    , ( 2, 22 )
    , ( 3, 22 )
    , ( 20, 22 )
    , ( 21, 22 )
    , ( 22, 22 )
    , ( 23, 22 )
    , ( 24, 22 )
    , ( 25, 22 )
    , ( 26, 22 )
    , ( 27, 22 )
    , ( 28, 22 )
    , ( 29, 22 )
    , ( 30, 22 )
    , ( 31, 22 )
    , ( 32, 22 )
    , ( 33, 22 )
    , ( 34, 22 )
    , ( 35, 22 )
    , ( 36, 22 )
    , ( 37, 22 )
    , ( 38, 22 )
    , ( 39, 22 )
    , ( 40, 22 )
    , ( 41, 22 )
    , ( 42, 22 )
    , ( 43, 22 )
    , ( 44, 22 )
    , ( 45, 22 )
    , ( 46, 22 )
    , ( 47, 22 )
    , ( 48, 22 )
    , ( 49, 22 )
    , ( 50, 22 )
    , ( 51, 22 )
    , ( 52, 22 )
    , ( 53, 22 )
    , ( 54, 22 )
    , ( 55, 22 )
    , ( 56, 22 )
    , ( 57, 22 )
    , ( 58, 22 )
    , ( 59, 22 )
    , ( 76, 22 )
    , ( 77, 22 )
    , ( 78, 22 )
    , ( 79, 22 )
    , ( 0, 23 )
    , ( 1, 23 )
    , ( 2, 23 )
    , ( 3, 23 )
    , ( 20, 23 )
    , ( 21, 23 )
    , ( 22, 23 )
    , ( 23, 23 )
    , ( 24, 23 )
    , ( 25, 23 )
    , ( 26, 23 )
    , ( 27, 23 )
    , ( 28, 23 )
    , ( 29, 23 )
    , ( 30, 23 )
    , ( 31, 23 )
    , ( 32, 23 )
    , ( 33, 23 )
    , ( 34, 23 )
    , ( 35, 23 )
    , ( 36, 23 )
    , ( 37, 23 )
    , ( 38, 23 )
    , ( 39, 23 )
    , ( 40, 23 )
    , ( 41, 23 )
    , ( 42, 23 )
    , ( 43, 23 )
    , ( 44, 23 )
    , ( 45, 23 )
    , ( 46, 23 )
    , ( 47, 23 )
    , ( 48, 23 )
    , ( 49, 23 )
    , ( 50, 23 )
    , ( 51, 23 )
    , ( 52, 23 )
    , ( 53, 23 )
    , ( 54, 23 )
    , ( 55, 23 )
    , ( 56, 23 )
    , ( 57, 23 )
    , ( 58, 23 )
    , ( 59, 23 )
    , ( 76, 23 )
    , ( 77, 23 )
    , ( 78, 23 )
    , ( 79, 23 )
    , ( 0, 24 )
    , ( 1, 24 )
    , ( 2, 24 )
    , ( 3, 24 )
    , ( 24, 24 )
    , ( 25, 24 )
    , ( 26, 24 )
    , ( 27, 24 )
    , ( 28, 24 )
    , ( 29, 24 )
    , ( 30, 24 )
    , ( 31, 24 )
    , ( 32, 24 )
    , ( 33, 24 )
    , ( 34, 24 )
    , ( 35, 24 )
    , ( 36, 24 )
    , ( 37, 24 )
    , ( 38, 24 )
    , ( 39, 24 )
    , ( 40, 24 )
    , ( 41, 24 )
    , ( 42, 24 )
    , ( 43, 24 )
    , ( 44, 24 )
    , ( 45, 24 )
    , ( 46, 24 )
    , ( 47, 24 )
    , ( 48, 24 )
    , ( 49, 24 )
    , ( 50, 24 )
    , ( 51, 24 )
    , ( 52, 24 )
    , ( 53, 24 )
    , ( 54, 24 )
    , ( 55, 24 )
    , ( 76, 24 )
    , ( 77, 24 )
    , ( 78, 24 )
    , ( 79, 24 )
    , ( 0, 25 )
    , ( 1, 25 )
    , ( 2, 25 )
    , ( 3, 25 )
    , ( 24, 25 )
    , ( 25, 25 )
    , ( 26, 25 )
    , ( 27, 25 )
    , ( 28, 25 )
    , ( 29, 25 )
    , ( 30, 25 )
    , ( 31, 25 )
    , ( 32, 25 )
    , ( 33, 25 )
    , ( 34, 25 )
    , ( 35, 25 )
    , ( 36, 25 )
    , ( 37, 25 )
    , ( 38, 25 )
    , ( 39, 25 )
    , ( 40, 25 )
    , ( 41, 25 )
    , ( 42, 25 )
    , ( 43, 25 )
    , ( 44, 25 )
    , ( 45, 25 )
    , ( 46, 25 )
    , ( 47, 25 )
    , ( 48, 25 )
    , ( 49, 25 )
    , ( 50, 25 )
    , ( 51, 25 )
    , ( 52, 25 )
    , ( 53, 25 )
    , ( 54, 25 )
    , ( 55, 25 )
    , ( 76, 25 )
    , ( 77, 25 )
    , ( 78, 25 )
    , ( 79, 25 )
    , ( 0, 26 )
    , ( 1, 26 )
    , ( 2, 26 )
    , ( 3, 26 )
    , ( 24, 26 )
    , ( 25, 26 )
    , ( 26, 26 )
    , ( 27, 26 )
    , ( 28, 26 )
    , ( 29, 26 )
    , ( 30, 26 )
    , ( 31, 26 )
    , ( 32, 26 )
    , ( 33, 26 )
    , ( 34, 26 )
    , ( 35, 26 )
    , ( 36, 26 )
    , ( 37, 26 )
    , ( 38, 26 )
    , ( 39, 26 )
    , ( 40, 26 )
    , ( 41, 26 )
    , ( 42, 26 )
    , ( 43, 26 )
    , ( 44, 26 )
    , ( 45, 26 )
    , ( 46, 26 )
    , ( 47, 26 )
    , ( 48, 26 )
    , ( 49, 26 )
    , ( 50, 26 )
    , ( 51, 26 )
    , ( 52, 26 )
    , ( 53, 26 )
    , ( 54, 26 )
    , ( 55, 26 )
    , ( 76, 26 )
    , ( 77, 26 )
    , ( 78, 26 )
    , ( 79, 26 )
    , ( 0, 27 )
    , ( 1, 27 )
    , ( 2, 27 )
    , ( 3, 27 )
    , ( 24, 27 )
    , ( 25, 27 )
    , ( 26, 27 )
    , ( 27, 27 )
    , ( 28, 27 )
    , ( 29, 27 )
    , ( 30, 27 )
    , ( 31, 27 )
    , ( 32, 27 )
    , ( 33, 27 )
    , ( 34, 27 )
    , ( 35, 27 )
    , ( 36, 27 )
    , ( 37, 27 )
    , ( 38, 27 )
    , ( 39, 27 )
    , ( 40, 27 )
    , ( 41, 27 )
    , ( 42, 27 )
    , ( 43, 27 )
    , ( 44, 27 )
    , ( 45, 27 )
    , ( 46, 27 )
    , ( 47, 27 )
    , ( 48, 27 )
    , ( 49, 27 )
    , ( 50, 27 )
    , ( 51, 27 )
    , ( 52, 27 )
    , ( 53, 27 )
    , ( 54, 27 )
    , ( 55, 27 )
    , ( 76, 27 )
    , ( 77, 27 )
    , ( 78, 27 )
    , ( 79, 27 )
    , ( 0, 28 )
    , ( 1, 28 )
    , ( 2, 28 )
    , ( 3, 28 )
    , ( 24, 28 )
    , ( 25, 28 )
    , ( 26, 28 )
    , ( 27, 28 )
    , ( 28, 28 )
    , ( 29, 28 )
    , ( 30, 28 )
    , ( 31, 28 )
    , ( 32, 28 )
    , ( 33, 28 )
    , ( 34, 28 )
    , ( 35, 28 )
    , ( 36, 28 )
    , ( 37, 28 )
    , ( 38, 28 )
    , ( 39, 28 )
    , ( 40, 28 )
    , ( 41, 28 )
    , ( 42, 28 )
    , ( 43, 28 )
    , ( 44, 28 )
    , ( 45, 28 )
    , ( 46, 28 )
    , ( 47, 28 )
    , ( 48, 28 )
    , ( 49, 28 )
    , ( 50, 28 )
    , ( 51, 28 )
    , ( 52, 28 )
    , ( 53, 28 )
    , ( 54, 28 )
    , ( 55, 28 )
    , ( 76, 28 )
    , ( 77, 28 )
    , ( 78, 28 )
    , ( 79, 28 )
    , ( 0, 29 )
    , ( 1, 29 )
    , ( 2, 29 )
    , ( 3, 29 )
    , ( 24, 29 )
    , ( 25, 29 )
    , ( 26, 29 )
    , ( 27, 29 )
    , ( 28, 29 )
    , ( 29, 29 )
    , ( 30, 29 )
    , ( 31, 29 )
    , ( 32, 29 )
    , ( 33, 29 )
    , ( 34, 29 )
    , ( 35, 29 )
    , ( 36, 29 )
    , ( 37, 29 )
    , ( 38, 29 )
    , ( 39, 29 )
    , ( 40, 29 )
    , ( 41, 29 )
    , ( 42, 29 )
    , ( 43, 29 )
    , ( 44, 29 )
    , ( 45, 29 )
    , ( 46, 29 )
    , ( 47, 29 )
    , ( 48, 29 )
    , ( 49, 29 )
    , ( 50, 29 )
    , ( 51, 29 )
    , ( 52, 29 )
    , ( 53, 29 )
    , ( 54, 29 )
    , ( 55, 29 )
    , ( 76, 29 )
    , ( 77, 29 )
    , ( 78, 29 )
    , ( 79, 29 )
    , ( 0, 30 )
    , ( 1, 30 )
    , ( 2, 30 )
    , ( 3, 30 )
    , ( 24, 30 )
    , ( 25, 30 )
    , ( 26, 30 )
    , ( 27, 30 )
    , ( 28, 30 )
    , ( 29, 30 )
    , ( 30, 30 )
    , ( 31, 30 )
    , ( 32, 30 )
    , ( 33, 30 )
    , ( 34, 30 )
    , ( 35, 30 )
    , ( 36, 30 )
    , ( 37, 30 )
    , ( 38, 30 )
    , ( 39, 30 )
    , ( 40, 30 )
    , ( 41, 30 )
    , ( 42, 30 )
    , ( 43, 30 )
    , ( 44, 30 )
    , ( 45, 30 )
    , ( 46, 30 )
    , ( 47, 30 )
    , ( 48, 30 )
    , ( 49, 30 )
    , ( 50, 30 )
    , ( 51, 30 )
    , ( 52, 30 )
    , ( 53, 30 )
    , ( 54, 30 )
    , ( 55, 30 )
    , ( 76, 30 )
    , ( 77, 30 )
    , ( 78, 30 )
    , ( 79, 30 )
    , ( 0, 31 )
    , ( 1, 31 )
    , ( 2, 31 )
    , ( 3, 31 )
    , ( 24, 31 )
    , ( 25, 31 )
    , ( 26, 31 )
    , ( 27, 31 )
    , ( 28, 31 )
    , ( 29, 31 )
    , ( 30, 31 )
    , ( 31, 31 )
    , ( 32, 31 )
    , ( 33, 31 )
    , ( 34, 31 )
    , ( 35, 31 )
    , ( 36, 31 )
    , ( 37, 31 )
    , ( 38, 31 )
    , ( 39, 31 )
    , ( 40, 31 )
    , ( 41, 31 )
    , ( 42, 31 )
    , ( 43, 31 )
    , ( 44, 31 )
    , ( 45, 31 )
    , ( 46, 31 )
    , ( 47, 31 )
    , ( 48, 31 )
    , ( 49, 31 )
    , ( 50, 31 )
    , ( 51, 31 )
    , ( 52, 31 )
    , ( 53, 31 )
    , ( 54, 31 )
    , ( 55, 31 )
    , ( 76, 31 )
    , ( 77, 31 )
    , ( 78, 31 )
    , ( 79, 31 )
    , ( 0, 32 )
    , ( 1, 32 )
    , ( 2, 32 )
    , ( 3, 32 )
    , ( 24, 32 )
    , ( 25, 32 )
    , ( 26, 32 )
    , ( 27, 32 )
    , ( 28, 32 )
    , ( 29, 32 )
    , ( 30, 32 )
    , ( 31, 32 )
    , ( 32, 32 )
    , ( 33, 32 )
    , ( 34, 32 )
    , ( 35, 32 )
    , ( 36, 32 )
    , ( 37, 32 )
    , ( 38, 32 )
    , ( 39, 32 )
    , ( 40, 32 )
    , ( 41, 32 )
    , ( 42, 32 )
    , ( 43, 32 )
    , ( 44, 32 )
    , ( 45, 32 )
    , ( 46, 32 )
    , ( 47, 32 )
    , ( 48, 32 )
    , ( 49, 32 )
    , ( 50, 32 )
    , ( 51, 32 )
    , ( 52, 32 )
    , ( 53, 32 )
    , ( 54, 32 )
    , ( 55, 32 )
    , ( 76, 32 )
    , ( 77, 32 )
    , ( 78, 32 )
    , ( 79, 32 )
    , ( 0, 33 )
    , ( 1, 33 )
    , ( 2, 33 )
    , ( 3, 33 )
    , ( 24, 33 )
    , ( 25, 33 )
    , ( 26, 33 )
    , ( 27, 33 )
    , ( 28, 33 )
    , ( 29, 33 )
    , ( 30, 33 )
    , ( 31, 33 )
    , ( 32, 33 )
    , ( 33, 33 )
    , ( 34, 33 )
    , ( 35, 33 )
    , ( 36, 33 )
    , ( 37, 33 )
    , ( 38, 33 )
    , ( 39, 33 )
    , ( 40, 33 )
    , ( 41, 33 )
    , ( 42, 33 )
    , ( 43, 33 )
    , ( 44, 33 )
    , ( 45, 33 )
    , ( 46, 33 )
    , ( 47, 33 )
    , ( 48, 33 )
    , ( 49, 33 )
    , ( 50, 33 )
    , ( 51, 33 )
    , ( 52, 33 )
    , ( 53, 33 )
    , ( 54, 33 )
    , ( 55, 33 )
    , ( 76, 33 )
    , ( 77, 33 )
    , ( 78, 33 )
    , ( 79, 33 )
    , ( 0, 34 )
    , ( 1, 34 )
    , ( 2, 34 )
    , ( 3, 34 )
    , ( 24, 34 )
    , ( 25, 34 )
    , ( 26, 34 )
    , ( 27, 34 )
    , ( 28, 34 )
    , ( 29, 34 )
    , ( 30, 34 )
    , ( 31, 34 )
    , ( 32, 34 )
    , ( 33, 34 )
    , ( 34, 34 )
    , ( 35, 34 )
    , ( 44, 34 )
    , ( 45, 34 )
    , ( 46, 34 )
    , ( 47, 34 )
    , ( 48, 34 )
    , ( 49, 34 )
    , ( 50, 34 )
    , ( 51, 34 )
    , ( 52, 34 )
    , ( 53, 34 )
    , ( 54, 34 )
    , ( 55, 34 )
    , ( 76, 34 )
    , ( 77, 34 )
    , ( 78, 34 )
    , ( 79, 34 )
    , ( 0, 35 )
    , ( 1, 35 )
    , ( 2, 35 )
    , ( 3, 35 )
    , ( 24, 35 )
    , ( 25, 35 )
    , ( 26, 35 )
    , ( 27, 35 )
    , ( 28, 35 )
    , ( 29, 35 )
    , ( 30, 35 )
    , ( 31, 35 )
    , ( 32, 35 )
    , ( 33, 35 )
    , ( 34, 35 )
    , ( 35, 35 )
    , ( 44, 35 )
    , ( 45, 35 )
    , ( 46, 35 )
    , ( 47, 35 )
    , ( 48, 35 )
    , ( 49, 35 )
    , ( 50, 35 )
    , ( 51, 35 )
    , ( 52, 35 )
    , ( 53, 35 )
    , ( 54, 35 )
    , ( 55, 35 )
    , ( 76, 35 )
    , ( 77, 35 )
    , ( 78, 35 )
    , ( 79, 35 )
    , ( 0, 36 )
    , ( 1, 36 )
    , ( 2, 36 )
    , ( 3, 36 )
    , ( 24, 36 )
    , ( 25, 36 )
    , ( 26, 36 )
    , ( 27, 36 )
    , ( 28, 36 )
    , ( 29, 36 )
    , ( 30, 36 )
    , ( 31, 36 )
    , ( 32, 36 )
    , ( 33, 36 )
    , ( 34, 36 )
    , ( 35, 36 )
    , ( 44, 36 )
    , ( 45, 36 )
    , ( 46, 36 )
    , ( 47, 36 )
    , ( 48, 36 )
    , ( 49, 36 )
    , ( 50, 36 )
    , ( 51, 36 )
    , ( 52, 36 )
    , ( 53, 36 )
    , ( 54, 36 )
    , ( 55, 36 )
    , ( 76, 36 )
    , ( 77, 36 )
    , ( 78, 36 )
    , ( 79, 36 )
    , ( 0, 37 )
    , ( 1, 37 )
    , ( 2, 37 )
    , ( 3, 37 )
    , ( 24, 37 )
    , ( 25, 37 )
    , ( 26, 37 )
    , ( 27, 37 )
    , ( 28, 37 )
    , ( 29, 37 )
    , ( 30, 37 )
    , ( 31, 37 )
    , ( 32, 37 )
    , ( 33, 37 )
    , ( 34, 37 )
    , ( 35, 37 )
    , ( 44, 37 )
    , ( 45, 37 )
    , ( 46, 37 )
    , ( 47, 37 )
    , ( 48, 37 )
    , ( 49, 37 )
    , ( 50, 37 )
    , ( 51, 37 )
    , ( 52, 37 )
    , ( 53, 37 )
    , ( 54, 37 )
    , ( 55, 37 )
    , ( 76, 37 )
    , ( 77, 37 )
    , ( 78, 37 )
    , ( 79, 37 )
    , ( 0, 38 )
    , ( 1, 38 )
    , ( 2, 38 )
    , ( 3, 38 )
    , ( 24, 38 )
    , ( 25, 38 )
    , ( 26, 38 )
    , ( 27, 38 )
    , ( 28, 38 )
    , ( 29, 38 )
    , ( 30, 38 )
    , ( 31, 38 )
    , ( 32, 38 )
    , ( 33, 38 )
    , ( 34, 38 )
    , ( 35, 38 )
    , ( 44, 38 )
    , ( 45, 38 )
    , ( 46, 38 )
    , ( 47, 38 )
    , ( 48, 38 )
    , ( 49, 38 )
    , ( 50, 38 )
    , ( 51, 38 )
    , ( 52, 38 )
    , ( 53, 38 )
    , ( 54, 38 )
    , ( 55, 38 )
    , ( 76, 38 )
    , ( 77, 38 )
    , ( 78, 38 )
    , ( 79, 38 )
    , ( 0, 39 )
    , ( 1, 39 )
    , ( 2, 39 )
    , ( 3, 39 )
    , ( 24, 39 )
    , ( 25, 39 )
    , ( 26, 39 )
    , ( 27, 39 )
    , ( 28, 39 )
    , ( 29, 39 )
    , ( 30, 39 )
    , ( 31, 39 )
    , ( 32, 39 )
    , ( 33, 39 )
    , ( 34, 39 )
    , ( 35, 39 )
    , ( 44, 39 )
    , ( 45, 39 )
    , ( 46, 39 )
    , ( 47, 39 )
    , ( 48, 39 )
    , ( 49, 39 )
    , ( 50, 39 )
    , ( 51, 39 )
    , ( 52, 39 )
    , ( 53, 39 )
    , ( 54, 39 )
    , ( 55, 39 )
    , ( 76, 39 )
    , ( 77, 39 )
    , ( 78, 39 )
    , ( 79, 39 )
    , ( 0, 40 )
    , ( 1, 40 )
    , ( 2, 40 )
    , ( 3, 40 )
    , ( 24, 40 )
    , ( 25, 40 )
    , ( 26, 40 )
    , ( 27, 40 )
    , ( 28, 40 )
    , ( 29, 40 )
    , ( 30, 40 )
    , ( 31, 40 )
    , ( 32, 40 )
    , ( 33, 40 )
    , ( 34, 40 )
    , ( 35, 40 )
    , ( 44, 40 )
    , ( 45, 40 )
    , ( 46, 40 )
    , ( 47, 40 )
    , ( 48, 40 )
    , ( 49, 40 )
    , ( 50, 40 )
    , ( 51, 40 )
    , ( 52, 40 )
    , ( 53, 40 )
    , ( 54, 40 )
    , ( 55, 40 )
    , ( 76, 40 )
    , ( 77, 40 )
    , ( 78, 40 )
    , ( 79, 40 )
    , ( 0, 41 )
    , ( 1, 41 )
    , ( 2, 41 )
    , ( 3, 41 )
    , ( 24, 41 )
    , ( 25, 41 )
    , ( 26, 41 )
    , ( 27, 41 )
    , ( 28, 41 )
    , ( 29, 41 )
    , ( 30, 41 )
    , ( 31, 41 )
    , ( 32, 41 )
    , ( 33, 41 )
    , ( 34, 41 )
    , ( 35, 41 )
    , ( 44, 41 )
    , ( 45, 41 )
    , ( 46, 41 )
    , ( 47, 41 )
    , ( 48, 41 )
    , ( 49, 41 )
    , ( 50, 41 )
    , ( 51, 41 )
    , ( 52, 41 )
    , ( 53, 41 )
    , ( 54, 41 )
    , ( 55, 41 )
    , ( 76, 41 )
    , ( 77, 41 )
    , ( 78, 41 )
    , ( 79, 41 )
    , ( 0, 42 )
    , ( 1, 42 )
    , ( 2, 42 )
    , ( 3, 42 )
    , ( 24, 42 )
    , ( 25, 42 )
    , ( 26, 42 )
    , ( 27, 42 )
    , ( 28, 42 )
    , ( 29, 42 )
    , ( 30, 42 )
    , ( 31, 42 )
    , ( 32, 42 )
    , ( 33, 42 )
    , ( 34, 42 )
    , ( 35, 42 )
    , ( 44, 42 )
    , ( 45, 42 )
    , ( 46, 42 )
    , ( 47, 42 )
    , ( 48, 42 )
    , ( 49, 42 )
    , ( 50, 42 )
    , ( 51, 42 )
    , ( 52, 42 )
    , ( 53, 42 )
    , ( 54, 42 )
    , ( 55, 42 )
    , ( 76, 42 )
    , ( 77, 42 )
    , ( 78, 42 )
    , ( 79, 42 )
    , ( 0, 43 )
    , ( 1, 43 )
    , ( 2, 43 )
    , ( 3, 43 )
    , ( 24, 43 )
    , ( 25, 43 )
    , ( 26, 43 )
    , ( 27, 43 )
    , ( 28, 43 )
    , ( 29, 43 )
    , ( 30, 43 )
    , ( 31, 43 )
    , ( 32, 43 )
    , ( 33, 43 )
    , ( 34, 43 )
    , ( 35, 43 )
    , ( 44, 43 )
    , ( 45, 43 )
    , ( 46, 43 )
    , ( 47, 43 )
    , ( 48, 43 )
    , ( 49, 43 )
    , ( 50, 43 )
    , ( 51, 43 )
    , ( 52, 43 )
    , ( 53, 43 )
    , ( 54, 43 )
    , ( 55, 43 )
    , ( 76, 43 )
    , ( 77, 43 )
    , ( 78, 43 )
    , ( 79, 43 )
    , ( 0, 44 )
    , ( 1, 44 )
    , ( 2, 44 )
    , ( 3, 44 )
    , ( 76, 44 )
    , ( 77, 44 )
    , ( 78, 44 )
    , ( 79, 44 )
    , ( 0, 45 )
    , ( 1, 45 )
    , ( 2, 45 )
    , ( 3, 45 )
    , ( 76, 45 )
    , ( 77, 45 )
    , ( 78, 45 )
    , ( 79, 45 )
    , ( 0, 46 )
    , ( 1, 46 )
    , ( 2, 46 )
    , ( 3, 46 )
    , ( 76, 46 )
    , ( 77, 46 )
    , ( 78, 46 )
    , ( 79, 46 )
    , ( 0, 47 )
    , ( 1, 47 )
    , ( 2, 47 )
    , ( 3, 47 )
    , ( 76, 47 )
    , ( 77, 47 )
    , ( 78, 47 )
    , ( 79, 47 )
    , ( 0, 48 )
    , ( 1, 48 )
    , ( 2, 48 )
    , ( 3, 48 )
    , ( 76, 48 )
    , ( 77, 48 )
    , ( 78, 48 )
    , ( 79, 48 )
    , ( 0, 49 )
    , ( 1, 49 )
    , ( 2, 49 )
    , ( 3, 49 )
    , ( 76, 49 )
    , ( 77, 49 )
    , ( 78, 49 )
    , ( 79, 49 )
    , ( 0, 50 )
    , ( 1, 50 )
    , ( 2, 50 )
    , ( 3, 50 )
    , ( 76, 50 )
    , ( 77, 50 )
    , ( 78, 50 )
    , ( 79, 50 )
    , ( 0, 51 )
    , ( 1, 51 )
    , ( 2, 51 )
    , ( 3, 51 )
    , ( 76, 51 )
    , ( 77, 51 )
    , ( 78, 51 )
    , ( 79, 51 )
    , ( 0, 52 )
    , ( 1, 52 )
    , ( 2, 52 )
    , ( 3, 52 )
    , ( 76, 52 )
    , ( 77, 52 )
    , ( 78, 52 )
    , ( 79, 52 )
    , ( 0, 53 )
    , ( 1, 53 )
    , ( 2, 53 )
    , ( 3, 53 )
    , ( 76, 53 )
    , ( 77, 53 )
    , ( 78, 53 )
    , ( 79, 53 )
    , ( 0, 54 )
    , ( 1, 54 )
    , ( 2, 54 )
    , ( 3, 54 )
    , ( 76, 54 )
    , ( 77, 54 )
    , ( 78, 54 )
    , ( 79, 54 )
    , ( 0, 55 )
    , ( 1, 55 )
    , ( 2, 55 )
    , ( 3, 55 )
    , ( 76, 55 )
    , ( 77, 55 )
    , ( 78, 55 )
    , ( 79, 55 )
    , ( 0, 56 )
    , ( 1, 56 )
    , ( 2, 56 )
    , ( 3, 56 )
    , ( 76, 56 )
    , ( 77, 56 )
    , ( 78, 56 )
    , ( 79, 56 )
    , ( 0, 57 )
    , ( 1, 57 )
    , ( 2, 57 )
    , ( 3, 57 )
    , ( 4, 57 )
    , ( 5, 57 )
    , ( 6, 57 )
    , ( 7, 57 )
    , ( 8, 57 )
    , ( 9, 57 )
    , ( 10, 57 )
    , ( 11, 57 )
    , ( 12, 57 )
    , ( 13, 57 )
    , ( 14, 57 )
    , ( 15, 57 )
    , ( 16, 57 )
    , ( 17, 57 )
    , ( 18, 57 )
    , ( 19, 57 )
    , ( 20, 57 )
    , ( 21, 57 )
    , ( 22, 57 )
    , ( 23, 57 )
    , ( 24, 57 )
    , ( 25, 57 )
    , ( 26, 57 )
    , ( 27, 57 )
    , ( 28, 57 )
    , ( 29, 57 )
    , ( 30, 57 )
    , ( 31, 57 )
    , ( 32, 57 )
    , ( 47, 57 )
    , ( 48, 57 )
    , ( 49, 57 )
    , ( 50, 57 )
    , ( 51, 57 )
    , ( 52, 57 )
    , ( 53, 57 )
    , ( 54, 57 )
    , ( 55, 57 )
    , ( 56, 57 )
    , ( 57, 57 )
    , ( 58, 57 )
    , ( 59, 57 )
    , ( 60, 57 )
    , ( 61, 57 )
    , ( 62, 57 )
    , ( 63, 57 )
    , ( 64, 57 )
    , ( 65, 57 )
    , ( 66, 57 )
    , ( 67, 57 )
    , ( 68, 57 )
    , ( 69, 57 )
    , ( 70, 57 )
    , ( 71, 57 )
    , ( 72, 57 )
    , ( 73, 57 )
    , ( 74, 57 )
    , ( 75, 57 )
    , ( 76, 57 )
    , ( 77, 57 )
    , ( 78, 57 )
    , ( 79, 57 )
    , ( 0, 58 )
    , ( 1, 58 )
    , ( 2, 58 )
    , ( 3, 58 )
    , ( 4, 58 )
    , ( 5, 58 )
    , ( 6, 58 )
    , ( 7, 58 )
    , ( 8, 58 )
    , ( 9, 58 )
    , ( 10, 58 )
    , ( 11, 58 )
    , ( 12, 58 )
    , ( 13, 58 )
    , ( 14, 58 )
    , ( 15, 58 )
    , ( 16, 58 )
    , ( 17, 58 )
    , ( 18, 58 )
    , ( 19, 58 )
    , ( 20, 58 )
    , ( 21, 58 )
    , ( 22, 58 )
    , ( 23, 58 )
    , ( 24, 58 )
    , ( 25, 58 )
    , ( 26, 58 )
    , ( 27, 58 )
    , ( 28, 58 )
    , ( 29, 58 )
    , ( 30, 58 )
    , ( 31, 58 )
    , ( 32, 58 )
    , ( 47, 58 )
    , ( 48, 58 )
    , ( 49, 58 )
    , ( 50, 58 )
    , ( 51, 58 )
    , ( 52, 58 )
    , ( 53, 58 )
    , ( 54, 58 )
    , ( 55, 58 )
    , ( 56, 58 )
    , ( 57, 58 )
    , ( 58, 58 )
    , ( 59, 58 )
    , ( 60, 58 )
    , ( 61, 58 )
    , ( 62, 58 )
    , ( 63, 58 )
    , ( 64, 58 )
    , ( 65, 58 )
    , ( 66, 58 )
    , ( 67, 58 )
    , ( 68, 58 )
    , ( 69, 58 )
    , ( 70, 58 )
    , ( 71, 58 )
    , ( 72, 58 )
    , ( 73, 58 )
    , ( 74, 58 )
    , ( 75, 58 )
    , ( 76, 58 )
    , ( 77, 58 )
    , ( 78, 58 )
    , ( 79, 58 )
    , ( 0, 59 )
    , ( 1, 59 )
    , ( 2, 59 )
    , ( 3, 59 )
    , ( 4, 59 )
    , ( 5, 59 )
    , ( 6, 59 )
    , ( 7, 59 )
    , ( 8, 59 )
    , ( 9, 59 )
    , ( 10, 59 )
    , ( 11, 59 )
    , ( 12, 59 )
    , ( 13, 59 )
    , ( 14, 59 )
    , ( 15, 59 )
    , ( 16, 59 )
    , ( 17, 59 )
    , ( 18, 59 )
    , ( 19, 59 )
    , ( 20, 59 )
    , ( 21, 59 )
    , ( 22, 59 )
    , ( 23, 59 )
    , ( 24, 59 )
    , ( 25, 59 )
    , ( 26, 59 )
    , ( 27, 59 )
    , ( 28, 59 )
    , ( 29, 59 )
    , ( 30, 59 )
    , ( 31, 59 )
    , ( 32, 59 )
    , ( 47, 59 )
    , ( 48, 59 )
    , ( 49, 59 )
    , ( 50, 59 )
    , ( 51, 59 )
    , ( 52, 59 )
    , ( 53, 59 )
    , ( 54, 59 )
    , ( 55, 59 )
    , ( 56, 59 )
    , ( 57, 59 )
    , ( 58, 59 )
    , ( 59, 59 )
    , ( 60, 59 )
    , ( 61, 59 )
    , ( 62, 59 )
    , ( 63, 59 )
    , ( 64, 59 )
    , ( 65, 59 )
    , ( 66, 59 )
    , ( 67, 59 )
    , ( 68, 59 )
    , ( 69, 59 )
    , ( 70, 59 )
    , ( 71, 59 )
    , ( 72, 59 )
    , ( 73, 59 )
    , ( 74, 59 )
    , ( 75, 59 )
    , ( 76, 59 )
    , ( 77, 59 )
    , ( 78, 59 )
    , ( 79, 59 )
    ]


createBlock : BlockLocation -> Block
createBlock ( x, y ) =
    Block "yellow" (toFloat x * 10.0) (toFloat y * 10.0) 10.0 10.0



-- VIEW


view : Screen -> Svg a
view screen =
    screen
        |> List.map createBlock
        |> List.map viewBlock
        |> Svg.g []


viewBlock : Block -> Svg a
viewBlock block =
    Svg.rect
        [ Svg.Attributes.fill <| block.color
        , Svg.Attributes.x <| String.fromFloat block.x
        , Svg.Attributes.y <| String.fromFloat block.y
        , Svg.Attributes.width <| String.fromFloat block.width
        , Svg.Attributes.height <| String.fromFloat block.height
        ]
        []
