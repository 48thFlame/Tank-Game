module Main exposing (main)

import Browser
import Browser.Events as Events
import Entity exposing (..)
import Html
import Json.Decode as Decode
import Set
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
    { tank : Tank
    , keys : KeysPressed
    }


initialModel : () -> ( Model, Cmd Msg )
initialModel _ =
    ( { tank = initialTank, keys = initialKeysDown }
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
            ( { model | tank = move model.tank delta }, Cmd.none )

        KeyDown key ->
            ( applyFuncToModelKeys model (addKey key), Cmd.none )

        KeyUp key ->
            ( applyFuncToModelKeys model (removeKey key), Cmd.none )

        Blur _ ->
            let
                _ =
                    Debug.log "here" model.keys
            in
            ( applyFuncToModelKeys model clearKeys, Cmd.none )



-- ( keysUpdate model eMsg, Cmd.none )
-- AccUp ->
--     ( { model | tank = actAccUp model.tank 10 10 }, Cmd.none )
-- AccDown ->
--     ( { model | tank = actAccDown model.tank 10 10 }, Cmd.none )
-- VIEW


view : Model -> Svg.Svg Msg
view model =
    Html.div []
        [ svg
            [ SvgAttrs.width "500"
            , SvgAttrs.height "500"
            , SvgAttrs.viewBox "0 0 500 500"
            , SvgAttrs.style "background: #efefef"
            ]
            [ viewEntity model.tank 10 ]
        , Html.div [] [ Html.text (model.keys |> Debug.toString) ]
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


type alias KeysPressed =
    Set.Set String


initialKeysDown : Set.Set String
initialKeysDown =
    Set.empty


addKey : String -> KeysPressed -> KeysPressed
addKey key s =
    Set.insert key s


removeKey : String -> KeysPressed -> KeysPressed
removeKey key s =
    Set.remove key s


clearKeys : KeysPressed -> KeysPressed
clearKeys _ =
    Set.empty


isKeyDown : String -> KeysPressed -> Bool
isKeyDown key s =
    Set.member key s


{-| Takes in a msg that holds a `String`
-}
keyDecoder : (String -> Msg) -> Decode.Decoder Msg
keyDecoder m =
    Decode.map m (Decode.field "key" Decode.string)


applyFuncToModelKeys : Model -> (KeysPressed -> KeysPressed) -> Model
applyFuncToModelKeys model func =
    { model | keys = func model.keys }



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
