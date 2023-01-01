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
    EntityBase


initialTank : Tank
initialTank =
    { pos = initialPosition
    , dim = initialDimension 64 64
    , rot = initialRotation
    , img = "assets/tank.png"
    }


type alias Model =
    { tank : Tank
    , keys : KeysPressed
    , fps : Int
    }


initialModel : () -> ( Model, Cmd Msg )
initialModel _ =
    ( { tank = initialTank, keys = initialKeysPressed, fps = 0 }
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
            let
                delta =
                    deltaTime / 1000
            in
            ( { model | tank = updateTank delta model model.tank }, Cmd.none )

        KeyDown key ->
            ( applyFuncToModelKeys model (addKey key), Cmd.none )

        KeyUp key ->
            ( applyFuncToModelKeys model (removeKey key), Cmd.none )

        Blur _ ->
            ( applyFuncToModelKeys model clearKeys, Cmd.none )


updateTank : Float -> Model -> Tank -> Tank
updateTank delta model tank =
    let
        speed =
            500 * delta

        rotationAngle =
            360 * delta

        stuff =
            [ ( "ArrowUp", MoveUpDown -speed )
            , ( "ArrowDown", MoveUpDown speed )
            , ( "ArrowLeft", Rotate -rotationAngle )
            , ( "ArrowRight", Rotate rotationAngle )
            ]

        handle : Model -> ( String, EntityAction ) -> EntityBase -> EntityBase
        handle m pair t =
            if isPressed (Tuple.first pair) m.keys then
                actAction (Tuple.second pair) t

            else
                t
    in
    List.foldl (handle model) tank stuff



-- VIEW


view : Model -> Svg.Svg Msg
view model =
    let
        width =
            "640"

        height =
            "480"
    in
    Html.div []
        [ svg
            [ SvgA.viewBox ("0 0 " ++ width ++ " " ++ height)
            , SvgA.width width
            , SvgA.height height
            , SvgA.style "background: #efefef; display: block; margin: auto;"
            ]
            [ viewEntity model.tank 1 ]
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
