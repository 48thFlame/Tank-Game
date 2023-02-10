module Game exposing (..)

import Constants exposing (..)
import Engine exposing (..)
import Svg
import Svg.Attributes as SvgA



-- "Model"


type alias GameState =
    { tank : Tank, enemy : Enemy }


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
    { tank = initialTank, enemy = newPlane }


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


type alias Enemy =
    { eb : EntityBase
    , projectiles : List Missile
    }


type alias Missile =
    { eb : EntityBase
    , sinceLaunch : Float
    }


newPlane : Enemy
newPlane =
    let
        planeEb =
            { pos =
                -- middle
                newPosition
                    ((width / 2) - (planeWidth / 2))
                    ((height / 2) - (planeHeight / 2))
            , dim = newDimension planeWidth planeHeight
            , rot = initialRotation
            , img = "assets/plane.png"
            }
    in
    { eb = planeEb
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
        , enemy = updateEnemy delta ran gs.tank gs.enemy
    }


updateEnemy : Float -> RandType -> Tank -> Enemy -> Enemy
updateEnemy delta ran tank e =
    { e
        | eb = updateEnemyEb ran e.eb
        , projectiles =
            newMissile (getCenterPos e.eb)
                :: e.projectiles
                |> List.map (updateMissile delta tank)
                |> filterMissiles
    }


updateEnemyEb : RandType -> EntityBase -> EntityBase
updateEnemyEb ran eb =
    let
        num1 =
            ran.a

        num2 =
            ran.b

        num3 =
            ran.c
    in
    if num3 > 0.6 then
        { eb
            | pos =
                newPosition
                    (getRandomInRange num1 0 (width - eb.dim.width + 1))
                    (getRandomInRange num2 0 (height - eb.dim.height + 1))
        }

    else
        eb


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
    let
        mEb =
            m.eb

        tEb =
            tank.eb

        mPos =
            getCenterPos mEb

        tPos =
            getCenterPos tEb

        yMy =
            tPos.y - mPos.y

        xMx =
            tPos.x - mPos.x

        radians =
            atan2 yMy xMx
    in
    radToDeg radians


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
        [ viewObj gs.tank, viewObj gs.enemy ]


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
