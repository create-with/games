module Util.Keyboard exposing
    ( Controls
    , initialKeys
    , keyDecoder
    , playerPressedArrowDownKey
    , playerPressedArrowUpKey
    , playerPressedKey
    , playerPressedSpacebarKey
    , validKeys
    )

-- IMPORTS

import Json.Decode exposing (Decoder)
import Set exposing (Set)



-- MODEL


type alias Controls =
    Set String



-- INIT


initialKeys : Controls
initialKeys =
    Set.empty



-- DECODER


keyDecoder : Decoder String
keyDecoder =
    Json.Decode.field "key" Json.Decode.string



-- VALIDATION


validKeys : Set String
validKeys =
    Set.empty
        |> Set.insert "ArrowUp"
        |> Set.insert "ArrowDown"
        |> Set.insert " "



-- HELPERS


playerPressedKey : Set String -> Bool
playerPressedKey =
    Set.isEmpty >> not


playerPressedSpacebarKey : Set String -> Bool
playerPressedSpacebarKey =
    Set.member " "


playerPressedArrowUpKey : Set String -> Bool
playerPressedArrowUpKey =
    Set.member "ArrowUp"


playerPressedArrowDownKey : Set String -> Bool
playerPressedArrowDownKey =
    Set.member "ArrowDown"
