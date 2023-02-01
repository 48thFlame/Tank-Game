module Game exposing (..)

import Constants exposing (..)
import Entity exposing (..)
import Keys exposing (KeysPressed)
import Svg


type alias Bullet =
    { eb : EntityBase
    , vel : Velocity
    }


type alias Tank =
    { eb : EntityBase
    , keys : KeyActionManager TankMsg
    , bullets : List Bullet
    , coolDown : Float
    }


initialTank : Tank
initialTank =
    { eb =
        { pos = { x = 0, y = 0 }
        , dim = newDimension 64 64
        , rot = initialRotation
        , img = "assets/tank.png"
        }
    , keys =
        [ ( "ArrowUp", Action (MoveForward tankSpeed) ), ( "ArrowLeft", Action (Rotate -tankRotationSpeed) ), ( "ArrowRight", Action (Rotate tankRotationSpeed) ), ( "ArrowDown", Fire ) ]
    , bullets = []
    , coolDown = 0
    }


newBullet : Position -> Float -> Bullet
newBullet pos rot =
    let
        xy =
            getXY 4 rot
    in
    { eb =
        { pos = pos
        , dim = newDimension 16 16
        , rot = rot
        , img = "assets/bullet.png"
        }
            |> actAction 1 (MoveForward 32)
    , vel = { dx = Tuple.first xy, dy = Tuple.second xy }
    }


type TankMsg
    = Action EntityAction
    | Fire


{-| Returns all `TankMsg`s
-}
getTankMsgs : { m | keys : KeysPressed } -> Tank -> List TankMsg
getTankMsgs model tank =
    keyManagerUpdate model.keys tank


msgsMapTankUpdate : Float -> { m | keys : KeysPressed } -> Tank -> Tank
msgsMapTankUpdate delta model tank =
    -- Gets all `TankMsg`s and calls `updateTank` one at a time updating the `tank`
    List.foldl (msgUpdateTank delta) tank (getTankMsgs model tank)


{-| Similar to `update` in every Elm program, just for the tank
-}
msgUpdateTank : Float -> TankMsg -> Tank -> Tank
msgUpdateTank delta tMsg tank =
    case tMsg of
        Action entAction ->
            { tank | eb = actAction delta entAction tank.eb }

        Fire ->
            if canFire tank delta tankGoodShotTime then
                tankFire tank

            else
                tank


canFire : Tank -> Float -> Float -> Bool
canFire tank delta timeGood =
    tank.coolDown + delta >= timeGood


tankFire : Tank -> Tank
tankFire tank =
    { tank
        | bullets = newBullet (getCenterPos tank.eb) tank.eb.rot :: tank.bullets
        , coolDown = 0
    }


updateTankBullets : Float -> Tank -> Tank
updateTankBullets delta tank =
    { tank
        | bullets =
            moveBullets delta tank.bullets
                |> removeBulletsOutside (newDimension width height)
        , coolDown = tank.coolDown + tankAddToShotCoolDown
    }


moveBullets : Float -> List Bullet -> List Bullet
moveBullets delta lb =
    List.map (applyVelocity (bulletSpeed * delta)) lb


removeBulletsOutside : Dimension -> List Bullet -> List Bullet
removeBulletsOutside dim lb =
    -- make the "canvas" to entity and check collision
    let
        canvasEnt =
            { dim = dim, pos = initialPosition, rot = initialRotation, img = "" }
    in
    List.filter
        (\b ->
            isCollided canvasEnt b.eb
        )
        lb


mainToCallUpdateTank : Float -> { m | keys : KeysPressed } -> Tank -> Tank
mainToCallUpdateTank delta model tank =
    msgsMapTankUpdate delta model tank |> updateTankBullets delta


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
