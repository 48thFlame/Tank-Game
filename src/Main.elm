module Main exposing (main)

import Browser
import Browser.Events as Events
import Constants exposing (..)
import Entity exposing (..)
import Html
import Keys exposing (..)
import Set
import Svg
import Svg.Attributes as SvgAttr



-- MODEL


type alias Tank =
    { eb : EntityBase
    , keys : KeyActionManager
    }


initialTank : Tank
initialTank =
    { eb =
        { pos = initialPosition
        , dim = initialDimension 64 64
        , rot = initialRotation
        , img = "assets/tank.png"
        }
    , keys =
        [ ( "ArrowUp", MoveForward tankSpeed )
        , ( "ArrowDown", MoveForward -tankSpeed )
        , ( "ArrowLeft", Rotate -tankRotationSpeed )
        , ( "ArrowRight", Rotate tankRotationSpeed )
        ]
    }


initialTank2 : Tank
initialTank2 =
    { eb =
        { pos = initialPosition
        , dim = initialDimension 64 64
        , rot = initialRotation
        , img = "assets/tank.png"
        }
    , keys =
        [ ( "w", MoveForward tankSpeed )
        , ( "s", MoveForward -tankSpeed )
        , ( "a", Rotate -tankRotationSpeed )
        , ( "d", Rotate tankRotationSpeed )
        ]
    }


type alias Model =
    { tank : Tank
    , tank2 : Tank
    , keys : KeysPressed
    , fps : Int
    , colliding : Bool
    }


initialModel : () -> ( Model, Cmd Msg )
initialModel _ =
    ( { tank = initialTank
      , tank2 = initialTank2
      , keys = initialKeysPressed
      , fps = 0
      , colliding = False
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = OnAnimationFrame Float
    | KeyDown String
    | KeyUp String
    | Blur Events.Visibility


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnAnimationFrame deltaTime ->
            -- main game loop
            let
                delta =
                    deltaTime / 1000
            in
            ( { model
                | tank = updateTank delta model model.tank
                , tank2 = updateTank delta model model.tank2
                , colliding =
                    isCollided
                        tankCollisionFriendliness
                        model.tank.eb
                        model.tank2.eb
              }
            , Cmd.none
            )

        KeyDown key ->
            ( applyFuncToModelKeys model (addKey key), Cmd.none )

        KeyUp key ->
            ( applyFuncToModelKeys model (removeKey key), Cmd.none )

        Blur _ ->
            ( applyFuncToModelKeys model clearKeys, Cmd.none )


updateTank : Float -> Model -> Tank -> Tank
updateTank delta model tank =
    keyManagerUpdate delta model.keys tank



-- VIEW


view : Model -> Svg.Svg Msg
view model =
    Html.div []
        [ gameCanvas model
        , Html.div [] [ Html.text (model.keys |> Set.toList |> String.join ", ") ]
        , Html.div [] [ Html.text (model.colliding |> Debug.toString) ]
        ]


gameCanvas : Model -> Svg.Svg msg
gameCanvas model =
    Svg.svg
        [ SvgAttr.viewBox ("0 0 " ++ width ++ " " ++ height)
        , SvgAttr.width width
        , SvgAttr.height height
        , SvgAttr.style "background: #efefef; display: block; margin: auto;"
        ]
        [ viewEntity model.tank.eb 1
        , viewEntity model.tank2.eb 1
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Events.onAnimationFrameDelta OnAnimationFrame
        , Events.onKeyDown (keyDecoder KeyDown)
        , Events.onKeyUp (keyDecoder KeyUp)
        , Events.onVisibilityChange Blur
        ]



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
