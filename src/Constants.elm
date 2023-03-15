module Constants exposing (..)

-- tank


tankWidth : Float
tankWidth =
    64


tankHeight : Float
tankHeight =
    64


tankSpeed : Float
tankSpeed =
    150


tankRotationSpeed : Float
tankRotationSpeed =
    160


tankCoolDown : Float
tankCoolDown =
    0.25



-- tank - bullet


bulletWidth : Float
bulletWidth =
    16


bulletHeight : Float
bulletHeight =
    16


bulletSpeed : Float
bulletSpeed =
    160



-- boss


bossWidth : Float
bossWidth =
    128


bossHeight : Float
bossHeight =
    128


bossSpeed : Float
bossSpeed =
    120


bossDestBuffer : Float
bossDestBuffer =
    80


boosShotCoolDown : Float
boosShotCoolDown =
    0.96



-- boss - healthBar


initialHealth : Float
initialHealth =
    100


barHeight : Float
barHeight =
    8



-- boss - missile


missileWidth : Float
missileWidth =
    48


missileHeight : Float
missileHeight =
    48


missileMaxTime : Float
missileMaxTime =
    5.8


missileAddToTime : Float
missileAddToTime =
    1


missileSpeed : Float
missileSpeed =
    80


missileDamage : Float
missileDamage =
    20


width : Float
width =
    780


height : Float
height =
    640
