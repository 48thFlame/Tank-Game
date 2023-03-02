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


tankGoodShotTime : Float
tankGoodShotTime =
    10


tankAddToShotCoolDown : Float
tankAddToShotCoolDown =
    50



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
    60


boosShotCoolDown : Float
boosShotCoolDown =
    0.96



-- boss - missile


missileMaxTime : Float
missileMaxTime =
    4.5


missileAddToTime : Float
missileAddToTime =
    1


missileSpeed : Float
missileSpeed =
    80


width : Float
width =
    780


height : Float
height =
    640
