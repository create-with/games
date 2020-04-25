module Util.List exposing (flatten)


flatten : List (List a) -> List a
flatten =
    List.foldr (++) []
