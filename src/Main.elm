module Main exposing (main)

import Browser
import Browser.Events as Events
import Engine exposing (..)
import Game exposing (..)
import Html
import Set
import Svg



-- MODEL


type alias Model =
    { gs : GameState
    , keys : KeysPressed
    }


initialModel : () -> ( Model, Cmd Msg )
initialModel _ =
    ( { gs = newGameState
      , keys = initialKeysPressed
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
            ( { model | gs = updateGameState delta model.keys model.gs }
            , Cmd.none
            )

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


view : Model -> Svg.Svg Msg
view model =
    Html.div []
        [ gameCanvas model.gs
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
