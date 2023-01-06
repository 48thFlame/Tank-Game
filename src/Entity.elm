module Entity exposing (..)

import Keys exposing (..)
import Svg
import Svg.Attributes as SvgA


type alias EntityBase =
    { pos : Position
    , dim : Dimension
    , rot : Rotation
    , img : Image
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


initialPosition : Position
initialPosition =
    { x = 320, y = 240 }


initialDimension : Float -> Float -> Dimension
initialDimension imageWidth imageHeight =
    { width = imageWidth, height = imageHeight }


initialRotation : Rotation
initialRotation =
    0


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
                radii =
                    degrees ent.rot

                x =
                    ent.pos.x + (v * delta * cos radii)

                y =
                    ent.pos.y + (v * delta * sin radii)

                newPos =
                    { x = x, y = y }
            in
            { ent | pos = newPos }


viewEntity : EntityBase -> Float -> Svg.Svg msg
viewEntity ent scale =
    let
        centeredPos =
            { x = ent.pos.x - ((ent.dim.width * scale) / 2)
            , y = ent.pos.y - ((ent.dim.height * scale) / 2)
            }

        rotateString =
            "rotate("
                ++ String.fromFloat ent.rot
                ++ ","
                ++ String.fromFloat ent.pos.x
                ++ ","
                ++ String.fromFloat ent.pos.y
                ++ ")"
    in
    Svg.image
        [ SvgA.xlinkHref ent.img
        , SvgA.x (String.fromFloat centeredPos.x)
        , SvgA.y (String.fromFloat centeredPos.y)
        , SvgA.transform rotateString
        , SvgA.style "image-rendering: pixelated;"
        ]
        []


type alias KeyActionManager =
    List ( String, EntityAction )


keyManagerUpdate :
    Float
    -> KeysPressed
    -> { e | eb : EntityBase, keys : KeyActionManager }
    -> { e | eb : EntityBase, keys : KeyActionManager }
keyManagerUpdate delta keys ent =
    let
        handle : ( String, EntityAction ) -> EntityBase -> EntityBase
        handle pair eb =
            if isPressed (Tuple.first pair) keys then
                actAction delta (Tuple.second pair) eb

            else
                eb
    in
    { ent | eb = List.foldl handle ent.eb ent.keys }
