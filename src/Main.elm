module Main exposing (main)

import Browser
import Browser.Events as Events
import Constants exposing (height, width)
import Game exposing (..)
import Html
import Keys exposing (..)
import Set
import Svg
import Svg.Attributes as SvgAttr



-- MODEL


type alias Model =
    { tank : Tank
    , keys : KeysPressed
    }


initialModel : () -> ( Model, Cmd Msg )
initialModel _ =
    ( { tank = initialTank
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
            ( { model | tank = mainToCallUpdateTank delta model model.tank }
            , Cmd.none
            )

        KeyDown key ->
            ( applyFuncToModelKeys model (addKey key), Cmd.none )

        KeyUp key ->
            ( applyFuncToModelKeys model (removeKey key), Cmd.none )

        Blur _ ->
            ( applyFuncToModelKeys model clearKeys, Cmd.none )



-- VIEW


view : Model -> Svg.Svg Msg
view model =
    Html.div []
        [ gameCanvas model
        , Html.div [] [ Html.text (model.keys |> Set.toList |> String.join ", ") ]
        ]


gameCanvas : Model -> Svg.Svg msg
gameCanvas model =
    Svg.svg
        [ SvgAttr.viewBox ("0 0 " ++ width ++ " " ++ height)
        , SvgAttr.width width
        , SvgAttr.height height
        , SvgAttr.style "background: #efefef; display: block; margin: auto;"
        ]
        [ viewTank model.tank ]



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
