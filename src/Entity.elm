module Entity exposing (..)

import Keys exposing (..)
import Svg
import Svg.Attributes as SvgA



-- Base type


type alias EntityBase =
    { pos : Position -- Center of entity for outside callers but technically is the right top corner and the image is moved when displaying to the "centered coords"
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



-- Collision


{-| func (r Rect) Intersects(s Rect) bool {
return !(s.Max.X < r.Min.X ||
s.Min.X > r.Max.X ||
s.Max.Y < r.Min.Y ||
s.Min.Y > r.Max.Y)
}
-}
isCollided : Float -> EntityBase -> EntityBase -> Bool
isCollided friendliness e1 e2 =
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
            minX1
                + friendliness
                <= maxX2
                - friendliness
                && minX2
                + friendliness
                <= maxX1
                - friendliness

        -- Check if there is an overlap in the y-axis
        yOverlap =
            minY1
                + friendliness
                <= maxY2
                - friendliness
                && minY2
                + friendliness
                <= maxY1
                - friendliness
    in
    -- Return true if there is overlap in both the x-axis and y-axis
    xOverlap && yOverlap



-- Keyboard control


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
