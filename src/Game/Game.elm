module Game.Game exposing (..)

import Constants exposing (..)
import Engine exposing (..)
import Game.Boss exposing (..)
import Game.GameObjs exposing (..)
import Game.Tank exposing (..)
import Svg
import Svg.Attributes as SvgA


newGameState : GameState
newGameState =
    { tank = newTank, boss = newBoss }


updateGameState : Float -> Float -> KeysPressed -> GameState -> GameState
updateGameState delta rand keys gs =
    let
        updTank =
            updateTank delta keys gs.boss gs.tank

        updBoss =
            updateBoss delta rand updTank gs.boss
    in
    { gs
        | tank = updTank
        , boss = updBoss
    }


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
        [ viewHealthBar gs.tank, viewObj gs.tank, viewHealthBar gs.boss, viewObj gs.boss ]


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


viewHealthBar : { a | eb : EntityBase, health : Float } -> Svg.Svg msg
viewHealthBar boss =
    let
        bPos =
            boss.eb.pos

        rectWidth =
            100

        rectPos =
            newPosition (bPos.x + ((boss.eb.dim.width - rectWidth) / 2)) (bPos.y - 20)

        barX =
            SvgA.x (String.fromFloat rectPos.x)

        barY =
            SvgA.y (String.fromFloat rectPos.y)

        barWidthA =
            SvgA.width (sf initialHealth)

        barHeightA =
            SvgA.height (sf barHeight)

        sf : Float -> String
        sf f =
            String.fromFloat f
    in
    Svg.g []
        [ Svg.rect
            [ barX
            , barY
            , barWidthA
            , barHeightA
            , SvgA.style "fill: #00000000; stroke: #ad3f2c; stroke-width: 2;"
            ]
            []
        , Svg.rect
            [ barX
            , barY
            , barHeightA
            , SvgA.width (sf boss.health)
            , SvgA.style "fill: #f0573c"
            ]
            []
        ]
