module Engine exposing (..)

import Json.Decode as Decode
import Set
import Svg
import Svg.Attributes as SvgA


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



-- base entity type


type alias EntityBase =
    { pos : Position -- Top right corner
    , dim : Dimension
    , rot : Rotation
    , img : Image -- The image path, such as `assets/ent.png`
    }


type alias Position =
    { x : Float
    , y : Float
    }


type alias Dimension =
    { width : Float
    , height : Float
    }


type alias Rotation =
    Float


type alias Velocity =
    { dx : Float
    , dy : Float
    }


type alias Image =
    String


type MoveDirection
    = UpDown
    | LeftRight


type EntityAction
    = MoveUpDown Float
    | MoveLeftRight Float
    | Rotate Float
    | MoveForward Float


newPosition : Float -> Float -> Position
newPosition x y =
    { x = x, y = y }


newDimension : Float -> Float -> Dimension
newDimension imageWidth imageHeight =
    { width = imageWidth, height = imageHeight }


initialRotation : Rotation
initialRotation =
    0


initialVelocity : Velocity
initialVelocity =
    { dx = 0, dy = 0 }


applyVelocity :
    Float
    -> { a | eb : EntityBase, vel : Velocity }
    -> { a | eb : EntityBase, vel : Velocity }
applyVelocity delta ent =
    { ent
        | eb =
            actAction delta (MoveUpDown ent.vel.dy) ent.eb
                |> actAction delta (MoveLeftRight ent.vel.dx)
    }


getCenterPos : EntityBase -> Position
getCenterPos e =
    { x = e.pos.x + (e.dim.width / 2), y = e.pos.y + (e.dim.height / 2) }


actAction : Float -> EntityAction -> EntityBase -> EntityBase
actAction delta action ent =
    case action of
        MoveUpDown dy ->
            let
                oldPos =
                    ent.pos

                newPos =
                    { oldPos | y = oldPos.y + (dy * delta) }
            in
            { ent | pos = newPos }

        MoveLeftRight dx ->
            let
                oldPos =
                    ent.pos

                newPos =
                    { oldPos | x = oldPos.x + (dx * delta) }
            in
            { ent | pos = newPos }

        Rotate dAngle ->
            { ent | rot = ent.rot + (dAngle * delta) }

        MoveForward v ->
            let
                xy =
                    getXY v ent.rot

                x =
                    ent.pos.x + (delta * Tuple.first xy)

                y =
                    ent.pos.y + (delta * Tuple.second xy)

                newPos =
                    { x = x, y = y }
            in
            { ent | pos = newPos }


getXY : Float -> Float -> ( Float, Float )
getXY v rot =
    let
        radii =
            degrees rot
    in
    ( v * cos radii, v * sin radii )


viewEntity : EntityBase -> Svg.Svg msg
viewEntity ent =
    let
        centeredPos =
            { x = ent.pos.x + (ent.dim.width / 2)
            , y = ent.pos.y + (ent.dim.height / 2)
            }

        rotateString =
            "rotate("
                ++ String.fromFloat ent.rot
                ++ ","
                ++ String.fromFloat centeredPos.x
                ++ ","
                ++ String.fromFloat centeredPos.y
                ++ ")"
    in
    Svg.image
        [ SvgA.xlinkHref ent.img
        , SvgA.x (String.fromFloat ent.pos.x)
        , SvgA.y (String.fromFloat ent.pos.y)
        , SvgA.transform rotateString
        , SvgA.style "image-rendering: pixelated;"
        ]
        []



-- Collision


isCollided : EntityBase -> EntityBase -> Bool
isCollided e1 e2 =
    let
        minX1 =
            e1.pos.x

        maxX1 =
            minX1 + e1.dim.width

        minX2 =
            e2.pos.x

        maxX2 =
            minX2 + e2.dim.width

        minY1 =
            e1.pos.y

        maxY1 =
            minY1 + e1.dim.height

        minY2 =
            e2.pos.y

        maxY2 =
            minY2 + e2.dim.height

        -- Check if there is an overlap in the x-axis
        xOverlap =
            minX1 <= maxX2 && minX2 <= maxX1

        -- Check if there is an overlap in the y-axis
        yOverlap =
            minY1 <= maxY2 && minY2 <= maxY1
    in
    -- Return true if there is overlap in both the x-axis and y-axis
    xOverlap && yOverlap



-- Keyboard control


type alias KeyActionManager eMsg =
    List ( String, eMsg )


keyManagerUpdate :
    KeysPressed
    -> { e | keys : KeyActionManager eMsg }
    -> List eMsg
keyManagerUpdate keysPressed ent =
    let
        pairToEMsg : ( String, eMsg ) -> Maybe eMsg
        pairToEMsg pair =
            if isPressed (Tuple.first pair) keysPressed then
                Just (Tuple.second pair)

            else
                Nothing
    in
    List.filterMap pairToEMsg ent.keys
