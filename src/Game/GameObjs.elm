module Game.GameObjs exposing (..)

import Constants exposing (..)
import Engine exposing (..)





type alias GameState =
    { tank : Tank, boss : Boss }


newTank : Tank
newTank =
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
    , health = initialHealth
    }


type alias Tank =
    { eb : EntityBase
    , keys : KeyActionManager TankMsg
    , projectiles : List Bullet
    , coolDown : Float
    , health : Float
    }


type TankMsg
    = Action EntityAction
    | Fire
    | MissileHit


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


type alias Bullet =
    { eb : EntityBase
    , vel : Velocity
    }



-- boss


newBoss : Boss
newBoss =
    let
        startPos =
            -- middle
            newPosition
                ((width / 2) - (bossWidth / 2))
                ((height / 2) - (bossHeight / 2))

        bossEb =
            { pos = startPos
            , dim = newDimension bossWidth bossHeight
            , rot = 0
            , img = "assets/boss.png"
            }
    in
    { eb = bossEb
    , dest = startPos
    , dist = -1
    , coolDown = 0
    , projectiles = [ newMissile (getCenterPos bossEb) ]
    , health = initialHealth
    }


type alias Boss =
    { eb : EntityBase
    , dest : Position
    , dist : Float
    , coolDown : Float
    , projectiles : List Missile
    , health : Float
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


type alias Missile =
    { eb : EntityBase
    , sinceLaunch : Float
    }
