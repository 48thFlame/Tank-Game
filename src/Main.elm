module Main exposing (main)

import Browser
import Browser.Events
import Json.Decode as Decode
import Svg exposing (..)
import Svg.Attributes as SvgAttrs



-- MODEL


type alias Model =
    { pos : Position }


type alias Position =
    { x : Float
    , y : Float
    }


initialModel : () -> ( Model, Cmd Msg )
initialModel _ =
    ( { pos = { x = 100, y = 200 } }
    , Cmd.none
    )



-- UPDATE


type Msg
    = OnAnimationFrame Float
    | KeyDown TankAction


type TankAction
    = MoveUp
    | MoveDown


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnAnimationFrame deltaTime ->
            ( model, Cmd.none )

        KeyDown action ->
            let
                _ =
                    Debug.log "key pressed" action

                oldPos : Position
                oldPos =
                    model.pos

                newPosUp : Position
                newPosUp =
                    { oldPos | y = oldPos.y - 10 }

                -- (0,0) - top left
                newPosDown : Position
                newPosDown =
                    { oldPos | y = oldPos.y + 10 }
            in
            case action of
                MoveUp ->
                    ( { model | pos = newPosUp }, Cmd.none )

                MoveDown ->
                    ( { model | pos = newPosDown }, Cmd.none )



-- VIEW


view : Model -> Svg.Svg Msg
view model =
    svg
        [ SvgAttrs.width "500"
        , SvgAttrs.height "500"
        , SvgAttrs.viewBox "0 0 500 500"
        , SvgAttrs.style "background: #efefef"
        ]
        [ viewTank model.pos ]


viewTank : Position -> Svg.Svg Msg
viewTank pos =
    Svg.image
        [ SvgAttrs.class "rendered-image-class"
        , SvgAttrs.width "100"
        , SvgAttrs.height "100"
        , SvgAttrs.x (String.fromFloat pos.x)
        , SvgAttrs.y (String.fromFloat pos.y)
        , SvgAttrs.xlinkHref "assets/tank.png"
        ]
        []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta OnAnimationFrame
        , Browser.Events.onKeyDown (Decode.map KeyDown keyDecoder)
        ]


keyDecoder : Decode.Decoder TankAction
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.andThen keyToPlayerAction


keyToPlayerAction : String -> Decode.Decoder TankAction
keyToPlayerAction keyString =
    case keyString of
        "ArrowUp" ->
            Decode.succeed MoveUp

        "ArrowDown" ->
            Decode.succeed MoveDown

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
