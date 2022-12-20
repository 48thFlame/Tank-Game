module Main exposing (main)

import Browser
import Html
import Svg
import Svg.Attributes as SAttrs
import Svg.Lazy
import Time



-- MODEL


type alias Position =
    { x : Float
    , y : Float
    }


type alias Model =
    { pos : Position }


initialModel : () -> ( Model, Cmd msg )
initialModel _ =
    ( { pos = { x = 100, y = 200 } }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Tick _ ->
            let
                oldPos =
                    model.pos

                newPos =
                    { oldPos | x = oldPos.y, y = oldPos.x }
            in
            -- Game loop
            ( { model | pos = newPos }, Cmd.none )



-- VIEW


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Svg.Lazy.lazy gameObject model.pos ]


gameObject : Position -> Html.Html msg
gameObject pos =
    Svg.svg
        [ SAttrs.width "500"
        , SAttrs.height "500"
        ]
        [ Svg.image
            [ SAttrs.class "rendered-image-class"
            , SAttrs.width "100"
            , SAttrs.height "100"
            , SAttrs.x (String.fromFloat pos.x)
            , SAttrs.y (String.fromFloat pos.y)
            , SAttrs.xlinkHref "assets/tank.png"
            ]
            []
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    let
        fps : Float
        fps =
            1000 / 40

        -- 40 FPS
    in
    Sub.batch
        [ Time.every fps Tick
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
