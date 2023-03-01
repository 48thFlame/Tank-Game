module Game exposing (..)

import Constants exposing (..)
import Engine exposing (..)
import Svg
import Svg.Attributes as SvgA



-- "Model"


type alias GameState =
    { tank : Tank, boss : Boss }


type alias RandType =
    { a : Float
    , b : Float
    , c : Float
    }


type alias Bullet =
    { eb : EntityBase
    , vel : Velocity
    }


type alias Tank =
    { eb : EntityBase
    , keys : KeyActionManager TankMsg
    , projectiles : List Bullet
    , coolDown : Float
    }


newGameState : GameState
newGameState =
    { tank = initialTank, boss = newBoss }


initialTank : Tank
initialTank =
    { eb =
        { pos = newPosition (width - tankWidth - 24) (height - tankHeight - 24)
        , dim = newDimension tankWidth tankHeight
        , rot = -135
        , img = "assets/tank.png"
        }
    , keys =
        -- ! TODO: if press both double speed bug
        [ ( "ArrowUp", Action (MoveForward tankSpeed) )
        , ( "ArrowDown", Action (MoveForward -tankSpeed) )
        , ( "ArrowLeft", Action (Rotate -tankRotationSpeed) )
        , ( "ArrowRight", Action (Rotate tankRotationSpeed) )
        , ( "w", Action (MoveForward tankSpeed) )
        , ( "s", Action (MoveForward -tankSpeed) )
        , ( "a", Action (Rotate -tankRotationSpeed) )
        , ( "d", Action (Rotate tankRotationSpeed) )
        , ( " ", Fire )
        ]
    , projectiles = []
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


type alias Boss =
    { eb : EntityBase
    , dest : Position
    , dist : Float
    , projectiles : List Missile
    }


type alias Missile =
    { eb : EntityBase
    , sinceLaunch : Float
    }


newBoss : Boss
newBoss =
    let
        pos =
            newPosition
                ((width / 2) - (bossWidth / 2))
                ((height / 2) - (bossHeight / 2))

        dest =
            newPosition 100 100

        planeEb =
            { pos = pos

            -- middle
            , dim = newDimension bossWidth bossHeight
            , rot = getRotation pos dest
            , img = "assets/boss.png"
            }
    in
    { eb = planeEb
    , dest = dest
    , dist = getDistance dest planeEb.pos + bossDestBuffer
    , projectiles = [ newMissile (getCenterPos planeEb) ]
    }


newMissile : Position -> Missile
newMissile pos =
    { eb =
        { pos = pos
        , dim = newDimension 32 32
        , rot = initialRotation
        , img = "assets/missile.png"
        }
    , sinceLaunch = 0
    }



-- "Update"


updateGameState : Float -> RandType -> KeysPressed -> GameState -> GameState
updateGameState delta ran keys gs =
    { gs
        | tank = updateTank delta keys gs.tank
        , boss = updateBoss delta ran gs.tank gs.boss
    }


updateBoss : Float -> RandType -> Tank -> Boss -> Boss
updateBoss delta rand tank boss =
    let
        bEb =
            boss.eb

        shouldNewDest =
            boss.dist < 0

        newRot =
            getRotation bEb.pos boss.dest

        newDest =
            let
                _ =
                    Debug.log "here"
            in
            if shouldNewDest then
                newPosition
                    (getRandomInRange
                        rand.b
                        bossDestBuffer
                        (width - boss.eb.dim.width - bossDestBuffer)
                    )
                    (getRandomInRange rand.c bossDestBuffer (height - boss.eb.dim.height - bossDestBuffer))

            else
                boss.dest

        amountForward : Float
        amountForward =
            delta * bossSpeed

        _ =
            Debug.log "stuff:" { shouldNewDest = shouldNewDest, amountForward = amountForward, dist = boss.dist }
    in
    { boss
        | eb = faceRotation bEb newRot |> actAction delta (MoveForward bossSpeed)
        , dest = newDest
        , dist =
            if shouldNewDest then
                getDistance newDest bEb.pos

            else
                boss.dist - amountForward
        , projectiles =
            newMissile (getCenterPos boss.eb)
                :: boss.projectiles
                |> List.map (updateMissile delta tank)
                |> filterMissiles
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


filterMissiles : List Missile -> List Missile
filterMissiles lm =
    List.filter
        (\b ->
            b.sinceLaunch < missileMaxTime
        )
        lm


type TankMsg
    = Action EntityAction
    | Fire


updateTank : Float -> KeysPressed -> Tank -> Tank
updateTank delta keys tank =
    mapTankToMsgs delta (getTankMsgs keys tank) tank |> updateTankOutside |> updateTankBullets delta


{-| Returns all `TankMsg`s
-}
getTankMsgs : KeysPressed -> Tank -> List TankMsg
getTankMsgs keys tank =
    keyManagerUpdate keys tank


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
        , coolDown = tank.coolDown + (tankAddToShotCoolDown * delta)
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



-- "View"


gameCanvas : GameState -> Svg.Svg msg
gameCanvas gs =
    let
        stringedWidth =
            String.fromFloat width

        stringedHeight =
            String.fromFloat height
    in
    Svg.svg
        [ SvgA.viewBox ("0 0 " ++ stringedWidth ++ " " ++ stringedHeight)
        , SvgA.width stringedWidth
        , SvgA.height stringedHeight
        , SvgA.style "background: #efefef; display: block; margin: auto;"
        ]
        [ viewObj gs.tank, viewObj gs.boss ]


viewObj :
    { a
        | eb : EntityBase
        , projectiles : List { b | eb : EntityBase }
    }
    -> Svg.Svg msg
viewObj obj =
    let
        viewProjectile b =
            viewEntity b.eb

        projectilesSvg =
            List.map viewProjectile obj.projectiles
    in
    Svg.g []
        (viewEntity obj.eb :: projectilesSvg)
