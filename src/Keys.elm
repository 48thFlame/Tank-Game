module Keys exposing (..)

import Json.Decode as Decode
import Set


type alias KeysPressed =
    Set.Set String


initialKeysPressed : Set.Set String
initialKeysPressed =
    Set.empty


addKey : String -> KeysPressed -> KeysPressed
addKey key s =
    Set.insert key s


removeKey : String -> KeysPressed -> KeysPressed
removeKey key s =
    Set.remove key s


clearKeys : KeysPressed -> KeysPressed
clearKeys _ =
    Set.empty


isPressed : String -> KeysPressed -> Bool
isPressed key s =
    Set.member key s


{-| Takes in a msg that holds a `String`
-}
keyDecoder : (String -> msg) -> Decode.Decoder msg
keyDecoder m =
    Decode.map m (Decode.field "key" Decode.string)


applyFuncToModelKeys : { m | keys : KeysPressed } -> (KeysPressed -> KeysPressed) -> { m | keys : KeysPressed }
applyFuncToModelKeys model func =
    { model | keys = func model.keys }
