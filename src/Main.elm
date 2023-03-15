module Main exposing (main)

import Browser
import Browser.Events as Events
import Engine exposing (..)
import Game.Game exposing (..)
import Game.GameObjs exposing (..)
import Html
import Random



-- MODEL


newRandomGenerator : Cmd Msg
newRandomGenerator =
    Random.generate NewRandom (Random.float 0 1)


type alias Model =
    { gs : State
    , keys : KeysPressed
    , rand : Float
    }


initialModel : () -> ( Model, Cmd Msg )
initialModel _ =
    ( { gs = newGameState
      , keys = initialKeysPressed
      , rand = 0
      }
    , newRandomGenerator
    )



-- UPDATE


type Msg
    = OnAnimationFrame Float
    | NewRandom Float
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
            ( { model | gs = updateGameState delta model.rand model.keys model.gs }
            , newRandomGenerator
            )

        NewRandom rand ->
            ( { model | rand = rand }, Cmd.none )

        KeyDown key ->
            -- add key to model.keys
            ( applyFuncToModelKeys model (addKey key), Cmd.none )

        KeyUp key ->
            -- remove key from model.keys
            ( applyFuncToModelKeys model (removeKey key), Cmd.none )

        Blur _ ->
            -- clear model.keys
            ( applyFuncToModelKeys model clearKeys, Cmd.none )



-- VIEW


view : Model -> Html.Html Msg
view model =
    gameCanvas model.gs



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
