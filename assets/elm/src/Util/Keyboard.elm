module Util.Keyboard exposing
    ( Controls
    , initialKeys
    , keyDecoder
    , playerPressedArrowDownKey
    , playerPressedArrowLeftKey
    , playerPressedArrowRightKey
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
        |> Set.insert "ArrowDown"
        |> Set.insert "ArrowLeft"
        |> Set.insert "ArrowRight"
        |> Set.insert "ArrowUp"
        |> Set.insert " "



-- HELPERS


playerPressedKey : Set String -> Bool
playerPressedKey =
    Set.isEmpty >> not


playerPressedSpacebarKey : Set String -> Bool
playerPressedSpacebarKey =
    Set.member " "


playerPressedArrowDownKey : Set String -> Bool
playerPressedArrowDownKey =
    Set.member "ArrowDown"


playerPressedArrowLeftKey : Set String -> Bool
playerPressedArrowLeftKey =
    Set.member "ArrowLeft"


playerPressedArrowRightKey : Set String -> Bool
playerPressedArrowRightKey =
    Set.member "ArrowRight"


playerPressedArrowUpKey : Set String -> Bool
playerPressedArrowUpKey =
    Set.member "ArrowUp"
