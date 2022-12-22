module Main exposing (main)

-- import Time

import Browser
import Browser.Events
import Entity exposing (..)
import Json.Decode as Decode
import Svg exposing (..)
import Svg.Attributes as SvgAttrs



-- MODEL


type alias Tank =
    { pos : Position
    , vel : Velocity
    , rot : Rotation
    , img : String
    }


initialTank : Tank
initialTank =
    { pos = initialPosition
    , vel = initialVelocity
    , rot = 25
    , img = "assets/tank.png"
    }


type alias Model =
    { tank : Tank }


initialModel : () -> ( Model, Cmd Msg )
initialModel _ =
    ( { tank = initialTank }
    , Cmd.none
    )



-- UPDATE


type Msg
    = OnAnimationFrame Float
    | KeyDown EntityAction


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnAnimationFrame deltaTime ->
            let
                delta =
                    deltaTime / 1000
            in
            ( { model | tank = move model.tank delta }, Cmd.none )

        KeyDown action ->
            case action of
                AccUp ->
                    ( { model | tank = actAccUp model.tank 10 10 }, Cmd.none )

                AccDown ->
                    ( { model | tank = actAccDown model.tank 10 10 }, Cmd.none )



-- VIEW


view : Model -> Svg.Svg Msg
view model =
    svg
        [ SvgAttrs.width "500"
        , SvgAttrs.height "500"
        , SvgAttrs.viewBox "0 0 500 500"
        , SvgAttrs.style "background: #efefef"
        ]
        [ viewEntity model.tank 10 ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta OnAnimationFrame
        , Browser.Events.onKeyDown (Decode.map KeyDown keyDecoder)
        ]


keyDecoder : Decode.Decoder EntityAction
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.andThen keyToPlayerAction


keyToPlayerAction : String -> Decode.Decoder EntityAction
keyToPlayerAction keyString =
    case keyString of
        "ArrowUp" ->
            Decode.succeed AccUp

        "ArrowDown" ->
            Decode.succeed AccDown

        _ ->
            Decode.fail "not an event we care about"



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
