module Entity exposing (..)

import Svg
import Svg.Attributes as SvgA exposing (scale)


type alias Velocity =
    { dx : Float
    , dy : Float
    }


initialVelocity : Velocity
initialVelocity =
    { dx = 0, dy = 0 }


actAccUp : { e | vel : Velocity } -> Float -> Float -> { e | vel : Velocity }
actAccUp ent speed maxSpeed =
    let
        vel : Velocity
        vel =
            ent.vel

        tempVel : Velocity
        tempVel =
            { vel | dy = vel.dy - speed }

        newVel : Velocity
        newVel =
            if tempVel.dy < -maxSpeed then
                { vel | dy = maxSpeed }

            else
                tempVel
    in
    { ent | vel = newVel }


actAccDown : { e | vel : Velocity } -> Float -> Float -> { e | vel : Velocity }
actAccDown ent speed maxSpeed =
    let
        vel : Velocity
        vel =
            ent.vel

        tempVel : Velocity
        tempVel =
            { vel | dy = vel.dy + speed }

        newVel : Velocity
        newVel =
            if tempVel.dy > maxSpeed then
                { vel | dy = maxSpeed }

            else
                tempVel
    in
    { ent | vel = newVel }


type alias Position =
    { x : Float
    , y : Float
    }


initialPosition : Position
initialPosition =
    { x = 0, y = 0 }


move : { e | pos : Position, vel : Velocity } -> Float -> { e | pos : Position, vel : Velocity }
move ent delta =
    let
        pos : Position
        pos =
            ent.pos

        vel : Velocity
        vel =
            ent.vel

        newPos : Position
        newPos =
            { pos | x = pos.x + vel.dx * delta, y = pos.y + vel.dy * delta }
    in
    { ent | pos = newPos }


type alias Rotation =
    Float


initialRotation : Rotation
initialRotation =
    0


type EntityAction
    = AccUp
    | AccDown


viewEntity : { e | img : String, pos : Position, rot : Rotation } -> Float -> Svg.Svg msg
viewEntity ent scale =
    let
        scaleString : String
        scaleString =
            "scale(" ++ String.fromFloat scale ++ ")"

        rotateString : String
        rotateString =
            "rotate("
                ++ String.fromFloat ent.rot
                ++ ")"

        transformString =
            scaleString ++ " " ++ rotateString
    in
    Svg.image
        [ SvgA.class "rendered-image-class"
        , SvgA.xlinkHref ent.img
        , SvgA.x (String.fromFloat ent.pos.x)
        , SvgA.y (String.fromFloat ent.pos.y)
        , SvgA.transform transformString
        ]
        []
