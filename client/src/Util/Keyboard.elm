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

import Json.Decode
import Set



-- MODEL


type alias Controls =
    Set.Set String



-- INIT


initialKeys : Controls
initialKeys =
    Set.empty



-- DECODER


keyDecoder : Json.Decode.Decoder String
keyDecoder =
    Json.Decode.field "key" Json.Decode.string



-- VALIDATION


validKeys : Set.Set String
validKeys =
    Set.empty
        |> Set.insert "ArrowUp"
        |> Set.insert "ArrowDown"
        |> Set.insert " "



-- HELPERS


playerPressedKey : Set.Set String -> Bool
playerPressedKey =
    Set.isEmpty >> not


playerPressedSpacebarKey : Set.Set String -> Bool
playerPressedSpacebarKey =
    Set.member " "


playerPressedArrowUpKey : Set.Set String -> Bool
playerPressedArrowUpKey =
    Set.member "ArrowUp"


playerPressedArrowDownKey : Set.Set String -> Bool
playerPressedArrowDownKey =
    Set.member "ArrowDown"
