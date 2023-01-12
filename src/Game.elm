module Game exposing (..)

import Constants exposing (..)
import Entity exposing (..)
import Keys exposing (KeysPressed)
import Svg


type alias Tank =
    { eb : EntityBase
    , keys : KeyActionManager TankMsg
    , bullets : List Bullet
    }


type alias Bullet =
    { eb : EntityBase
    , vel : Velocity
    }


initialTank : Tank
initialTank =
    { eb =
        { pos = { x = 0, y = 0 }
        , dim = initialDimension 64 64
        , rot = initialRotation
        , img = "assets/tank.png"
        }
    , keys =
        [ ( "ArrowUp", Action (MoveForward tankSpeed) )
        , ( "ArrowLeft", Action (Rotate -tankRotationSpeed) )
        , ( "ArrowRight", Action (Rotate tankRotationSpeed) )
        , ( "ArrowDown", Fire )
        ]
    , bullets = []
    }


initialBullet : Bullet
initialBullet =
    { eb =
        { pos = initialPosition
        , dim = initialDimension 16 16
        , rot = initialRotation
        , img = "assets/bullet.png"
        }
    , vel = initialVelocity
    }


type TankMsg
    = Action EntityAction
    | Fire


{-| Returns all `TankMsg`s
-}
getTankMsgs : { m | keys : KeysPressed } -> Tank -> List TankMsg
getTankMsgs model tank =
    keyManagerUpdate model.keys tank


{-| Similar to `update` in every Elm program, just for the tank
-}
updateTank : Float -> TankMsg -> Tank -> Tank
updateTank delta tMsg tank =
    case tMsg of
        Action entAction ->
            { tank | eb = actAction delta entAction tank.eb }

        Fire ->
            { tank | bullets = initialBullet :: tank.bullets }


mainToCallUpdateTank : Float -> { m | keys : KeysPressed } -> Tank -> Tank
mainToCallUpdateTank delta model tank =
    -- Gets all `TankMsg`s and calls `updateTank` one at a time updating the `tank`
    List.foldl (updateTank delta) tank (getTankMsgs model tank)


viewTank : Tank -> Svg.Svg msg
viewTank tank =
    let
        viewBullet b =
            viewEntity b.eb

        bulletsSvg =
            List.map viewBullet tank.bullets
    in
    Svg.g []
        (viewEntity tank.eb :: bulletsSvg)
