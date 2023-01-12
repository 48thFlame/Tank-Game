module Entity exposing (..)

import Keys exposing (..)
import Svg
import Svg.Attributes as SvgA



-- Base type


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


initialPosition : Position
initialPosition =
    { x = 320, y = 240 }


initialDimension : Float -> Float -> Dimension
initialDimension imageWidth imageHeight =
    { width = imageWidth, height = imageHeight }


initialRotation : Rotation
initialRotation =
    0


initialVelocity : Velocity
initialVelocity =
    { dx = 0, dy = 0 }


applyVelocity :
    { a | eb : EntityBase, vel : Velocity }
    -> { a | eb : EntityBase, vel : Velocity }
applyVelocity ent =
    let
        oldPos =
            ent.eb.pos

        newPos =
            { oldPos
                | x = oldPos.x + ent.vel.dx
                , y = oldPos.y + ent.vel.dy
            }

        oldBase =
            ent.eb

        newBase =
            { oldBase | pos = newPos }
    in
    { ent | eb = newBase }


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
