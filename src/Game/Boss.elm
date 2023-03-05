module Game.Boss exposing (..)


import Constants exposing (..)
import Engine exposing (..)
import Game.GameObjs exposing (..)

updateBoss : Float -> Float -> Tank -> Boss -> Boss
updateBoss delta rand tank boss =
    let
        -- a =
        --     lcgRandom rand
        -- b =
        --     lcgRandom a
        bEb =
            boss.eb

        shouldNewDest =
            boss.dist < 0

        newRot =
            getRotation bEb.pos boss.dest

        newDest seed =
            if shouldNewDest then
                let
                    a =
                        lcgRandom seed

                    b =
                        lcgRandom a

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
                    newDest b

                else
                    pos

            else
                boss.dest

        amountForward : Float
        amountForward =
            delta * bossSpeed

        canShoot : Bool
        canShoot =
            boss.coolDown + delta > boosShotCoolDown

        projs : List Missile
        projs =
            if canShoot then
                newMissile (getCenterPos boss.eb)
                    :: boss.projectiles

            else
                boss.projectiles

        filterMTankHits t lm =
            List.filter (\m -> not (isCollided t.eb m.eb)) lm

        filterMissiles : List Missile -> List Missile
        filterMissiles lm =
            List.filter
                (\m ->
                    m.sinceLaunch < missileMaxTime
                )
                lm

        filteredProjs : List Missile
        filteredProjs =
            projs
                |> List.map (updateMissile delta tank)
                |> filterMTankHits tank
                |> filterMissiles

        dest =
            newDest rand
    in
    { boss
        | eb = faceRotation bEb newRot |> actAction delta (MoveForward bossSpeed)
        , dest = dest
        , dist =
            if shouldNewDest then
                getDistance dest bEb.pos

            else
                boss.dist - amountForward
        , projectiles = filteredProjs
        , coolDown =
            if canShoot then
                0

            else
                boss.coolDown + delta
    }


updateMissile : Float -> Tank -> Missile -> Missile
updateMissile delta tank m =
    let
        mEb =
            m.eb

        newMEb =
            -- get angle
            { mEb | rot = getMissileAngle tank m }
                -- move
                |> actAction delta (MoveForward missileSpeed)
    in
    { m | eb = newMEb, sinceLaunch = m.sinceLaunch + (missileAddToTime * delta) }


getMissileAngle : Tank -> Missile -> Float
getMissileAngle tank m =
    getRotation (getCenterPos m.eb) (getCenterPos tank.eb)
