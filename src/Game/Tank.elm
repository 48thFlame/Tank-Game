module Game.Tank exposing (..)

import Constants exposing (..)
import Engine exposing (..)
import Game.GameObjs exposing (..)


updateTank : Float -> KeysPressed -> Boss -> Tank -> Tank
updateTank delta keys boss tank =
    mapTankToMsgs delta (getTankMsgs keys boss tank) tank
        |> updateTankOutside
        |> updateTankBullets delta


{-| Returns all `TankMsg`s
-}
getTankMsgs : KeysPressed -> Boss -> Tank -> List TankMsg
getTankMsgs keys boss tank =
    List.concat [ tankGetHits boss tank, keyManagerUpdate keys tank ]


tankGetHits : Boss -> Tank -> List TankMsg
tankGetHits boss tank =
    let
        missileHitTank : Tank -> Missile -> Bool
        missileHitTank m t =
            isCollided m.eb t.eb
    in
    List.filter
        (missileHitTank tank)
        boss.projectiles
        |> List.map (\_ -> MissileHit)


{-| Using all msgs calls `updateTank` one at a time updating the `tank`
-}
mapTankToMsgs : Float -> List TankMsg -> Tank -> Tank
mapTankToMsgs delta msgs tank =
    List.foldl (msgUpdateTank delta) tank msgs


{-| Similar to `update` in every Elm program, just for the tank
-}
msgUpdateTank : Float -> TankMsg -> Tank -> Tank
msgUpdateTank delta tMsg tank =
    case tMsg of
        Action entAction ->
            { tank | eb = actAction delta entAction tank.eb }

        Fire ->
            if canFire tank delta tankCoolDown then
                tankFire tank

            else
                tank

        MissileHit ->
            { tank | health = tank.health - 10 }


canFire : Tank -> Float -> Float -> Bool
canFire tank delta timeGood =
    tank.coolDown + delta >= timeGood


tankFire : Tank -> Tank
tankFire tank =
    { tank
        | projectiles = newBullet (getCenterPos tank.eb) tank.eb.rot :: tank.projectiles
        , coolDown = 0
    }


updateTankOutside : Tank -> Tank
updateTankOutside tank =
    let
        tankEb =
            tank.eb

        tankX =
            if tankEb.pos.x < 0 then
                0

            else if (tankEb.pos.x + tankEb.dim.width) > width then
                width - tankEb.dim.width

            else
                tankEb.pos.x

        tankY =
            if tankEb.pos.y < 0 then
                0

            else if (tankEb.pos.y + tankEb.dim.height) > height then
                height - tankEb.dim.height

            else
                tankEb.pos.y

        newTankEb =
            { tankEb | pos = newPosition tankX tankY }
    in
    { tank | eb = newTankEb }


updateTankBullets : Float -> Tank -> Tank
updateTankBullets delta tank =
    { tank
        | projectiles =
            moveBullets delta tank.projectiles
                |> removeBulletsOutside (newDimension width height)
        , coolDown = tank.coolDown + delta
    }


moveBullets : Float -> List Bullet -> List Bullet
moveBullets delta lb =
    List.map (applyVelocity (bulletSpeed * delta)) lb


removeBulletsOutside : Dimension -> List Bullet -> List Bullet
removeBulletsOutside dim lb =
    -- make the "canvas" to entity and check collision
    let
        canvasEnt =
            { dim = dim, pos = newPosition 0 0, rot = initialRotation, img = "" }
    in
    List.filter
        (\b ->
            isCollided canvasEnt b.eb
        )
        lb
