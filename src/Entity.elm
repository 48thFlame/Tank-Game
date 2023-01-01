module Entity exposing (..)

import Html.Attributes exposing (action)
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
    | Rotate Float


initialPosition : Position
initialPosition =
    { x = 320, y = 240 }


initialDimension : Float -> Float -> Dimension
initialDimension imageWidth imageHeight =
    { width = imageWidth, height = imageHeight }


initialRotation : Rotation
initialRotation =
    0


actAction : EntityAction -> EntityBase -> EntityBase
actAction action ent =
    case action of
        MoveUpDown amount ->
            let
                oldPos =
                    ent.pos

                newPos =
                    { oldPos | y = oldPos.y + amount }
            in
            { ent | pos = newPos }

        Rotate angle ->
            { ent | rot = ent.rot + angle }


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
