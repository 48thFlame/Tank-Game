module Game.Game exposing (..)

import Constants exposing (..)
import Engine exposing (..)
import Game.Boss exposing (..)
import Game.GameObjs exposing (..)
import Game.Tank exposing (..)
import Html
import Html.Attributes as HtmlA
import Svg
import Svg.Attributes as SvgA


newGameState : State
newGameState =
    { ps = Playing, tank = newTank, boss = newBoss }


updateGameState : Float -> Float -> KeysPressed -> State -> State
updateGameState delta rand keys gs =
    case gs.ps of
        Playing ->
            let
                -- move missiles
                updLm =
                    updateBossMissiles delta gs.tank gs.boss

                -- update bullets
                updLb =
                    updateTankBullets delta gs.boss gs.tank

                -- update tank
                updTank =
                    updateTank delta keys updLm updLb

                -- filter missiles
                filteredLm =
                    filterBossMissiles gs.tank updLm

                -- update boss
                updBoss =
                    updateBoss delta rand filteredLm
            in
            { gs
                | tank = updTank
                , boss = updBoss
                , ps = getPlayingState gs
            }

        _ ->
            gs


getPlayingState : State -> PlayingState
getPlayingState gs =
    if gs.boss.health <= 0 then
        Won

    else if gs.tank.health <= 0 then
        Lost

    else
        Playing


gameCanvas : State -> Svg.Svg msg
gameCanvas gs =
    let
        stringedWidth =
            String.fromFloat width

        stringedHeight =
            String.fromFloat height

        mainGame =
            Svg.svg
                [ SvgA.viewBox ("0 0 " ++ stringedWidth ++ " " ++ stringedHeight)
                , SvgA.class "canvas"
                ]
                [ viewHealthBar gs.tank, viewObj gs.tank, viewHealthBar gs.boss, viewObj gs.boss ]
    in
    Html.div
        [ HtmlA.class "gameContainer" ]
        (case gs.ps of
            Playing ->
                [ mainGame ]

            Won ->
                [ Html.div [ SvgA.class "gameText" ] [ Html.text "You Win!" ], mainGame ]

            Lost ->
                [ Html.div [ SvgA.class "gameText" ] [ Html.text "You Lost!" ], mainGame ]
        )


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
