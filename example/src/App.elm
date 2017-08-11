module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Color exposing (Color)
import ColorPicker


type alias Model =
    { colorPicker : ColorPicker.State
    , colour : Color
    }


init : Model
init =
    { colorPicker = ColorPicker.empty
    , colour = Color.rgb 0 0 0
    }



-- UPDATE


type Msg
    = ColorPickerMsg ColorPicker.Msg


update : Msg -> Model -> Model
update message model =
    case message of
        ColorPickerMsg msg ->
            let
                ( m, colour ) =
                    ColorPicker.update msg model.colour model.colorPicker
            in
                { model
                    | colorPicker = m
                    , colour = colour |> Maybe.withDefault model.colour
                }



-- VIEW


view : Model -> Html Msg
view model =
    let
        hex =
            ColorPicker.color2Hex model.colour
    in
        div [ class "container" ]
            [ h1 [] [ text "Colour Picker" ]
            , div [] [ ColorPicker.view model.colour model.colorPicker |> Html.map ColorPickerMsg ]
            , div [] [ text hex ]
            , div
                [ style
                    [ ( "width", "50px" )
                    , ( "height", "50px" )
                    , ( "background-color", hex )
                    ]
                ]
                []
            ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = init
        , update = update
        , view = view
        }
