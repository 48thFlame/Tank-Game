module Game.Boss exposing (..)

import Constants exposing (..)
import Engine exposing (..)
import Game.GameObjs exposing (..)


{-| number between 0 and 1
-}
newRandDestBoss : Float -> Boss -> Position
newRandDestBoss seed boss =
    let
        a =
            lcgRandom seed

        b =
            lcgRandom a

        bEb =
            boss.eb

        pos =
            newPosition
                (getRandomInRange
                    a
                    bossDestBuffer
                    (width - boss.eb.dim.width - bossDestBuffer)
                )
                (getRandomInRange b bossDestBuffer (height - boss.eb.dim.height - bossDestBuffer))
    in
    -- if too close try again
    if getDistance pos bEb.pos < bossDestBuffer then
        newRandDestBoss b boss

    else
        pos


updateBoss : Float -> Float -> Boss -> Boss
updateBoss delta rand boss =
    let
        -- eb manipulation
        bEb =
            boss.eb

        newRot =
            getRotation bEb.pos boss.dest

        amountForward : Float
        amountForward =
            delta * bossSpeed

        newEb =
            faceRotation bEb newRot |> actAction delta (MoveForward bossSpeed)

        -- dest manipulation
        shouldNewDest =
            boss.dist < 0

        newDest =
            if shouldNewDest then
                newRandDestBoss rand boss

            else
                boss.dest

        newDist =
            if shouldNewDest then
                getDistance newDest bEb.pos

            else
                boss.dist - amountForward
    in
    { boss
        | eb = newEb
        , dest = newDest
        , dist = newDist
    }


canBossShoot : Float -> Boss -> Bool
canBossShoot delta boss =
    boss.coolDown + delta > boosShotCoolDown


filterMissilesTankHits : Tank -> List Missile -> List Missile
filterMissilesTankHits t lm =
    List.filter (\m -> not (isCollided t.eb m.eb)) lm


filterMissilesTimeOut : List Missile -> List Missile
filterMissilesTimeOut lm =
    List.filter
        (\m ->
            m.sinceLaunch < missileMaxTime
        )
        lm


updateMissile : Float -> Tank -> Missile -> Missile
updateMissile delta tank m =
    let
        mEb =
            m.eb

        newMEb =
            -- get angle
            getMissileAngle tank m
                -- face
                |> faceRotation mEb
                -- move
                |> actAction delta (MoveForward missileSpeed)

        {- | if hit a missile delete self by setting .sinceLaunch to deletion, otherwise just regular -}
        sinceLaunch =
            if List.any (\b -> isCollided m.eb b.eb) tank.projectiles then
                missileMaxTime + 1

            else
                m.sinceLaunch + (missileAddToTime * delta)
    in
    { m | eb = newMEb, sinceLaunch = sinceLaunch }


getMissileAngle : Tank -> Missile -> Float
getMissileAngle tank m =
    getRotation (getCenterPos m.eb) (getCenterPos tank.eb)


updateBossMissiles : Float -> Tank -> Boss -> Boss
updateBossMissiles delta tank boss =
    let
        -- missiles
        newProjs : List Missile
        newProjs =
            if canBossShoot delta boss then
                newMissile (getCenterPos boss.eb)
                    :: boss.projectiles

            else
                boss.projectiles

        newCoolDown =
            if canBossShoot delta boss then
                0

            else
                boss.coolDown + delta
    in
    { boss
        | projectiles =
            newProjs
                |> List.map (updateMissile delta tank)
        , coolDown = newCoolDown
    }


filterBossMissiles : Tank -> Boss -> Boss
filterBossMissiles tank boss =
    { boss | projectiles = filterMissilesTankHits tank boss.projectiles |> filterMissilesTimeOut }
