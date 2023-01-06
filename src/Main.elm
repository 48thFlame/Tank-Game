module Main exposing (main)

import Browser
import Browser.Events as Events
import Entity exposing (..)
import Html
import Keys exposing (..)
import Set
import Svg exposing (..)
import Svg.Attributes as SvgA



-- MODEL


type alias Tank =
    { eb : EntityBase
    , keys : KeyActionManager
    }


initialTank : Tank
initialTank =
    let
        speed =
            140

        rotationAngle =
            180
    in
    { eb =
        { pos = initialPosition
        , dim = initialDimension 64 64
        , rot = initialRotation
        , img = "assets/tank.png"
        }
    , keys =
        [ ( "ArrowUp", MoveForward speed )
        , ( "ArrowDown", MoveForward -speed )
        , ( "ArrowLeft", Rotate -rotationAngle )
        , ( "ArrowRight", Rotate rotationAngle )
        ]
    }


initialTank2 : Tank
initialTank2 =
    let
        speed =
            140

        rotationAngle =
            180
    in
    { eb =
        { pos = initialPosition
        , dim = initialDimension 64 64
        , rot = initialRotation
        , img = "assets/tank.png"
        }
    , keys =
        [ ( "w", MoveForward speed )
        , ( "s", MoveForward -speed )
        , ( "a", Rotate -rotationAngle )
        , ( "d", Rotate rotationAngle )
        ]
    }


type alias Model =
    { tank : Tank
    , tank2 : Tank
    , keys : KeysPressed
    , fps : Int
    }


initialModel : () -> ( Model, Cmd Msg )
initialModel _ =
    ( { tank = initialTank, tank2 = initialTank2, keys = initialKeysPressed, fps = 0 }
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
            ( { model | tank = updateTank delta model model.tank, tank2 = updateTank delta model model.tank2 }, Cmd.none )

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
    let
        width =
            "780"

        height =
            "640"
    in
    Html.div []
        [ svg
            [ SvgA.viewBox ("0 0 " ++ width ++ " " ++ height)
            , SvgA.width width
            , SvgA.height height
            , SvgA.style "background: #efefef; display: block; margin: auto;"
            ]
            [ viewEntity model.tank.eb 1
            , viewEntity model.tank2.eb 1
            ]
        , Html.div [] [ Html.text (model.keys |> Set.toList |> String.join ", ") ]
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
