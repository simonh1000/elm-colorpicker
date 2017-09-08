module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Color exposing (Color)
import ColorPicker


type alias Model =
    { colorPicker : ColorPicker.State
    , colorPickerHex : ColorPicker.State
    , hex : String
    , colour : Color
    }


init : Model
init =
    { colorPicker = ColorPicker.empty
    , colorPickerHex = ColorPicker.empty
    , hex = "#000000"
    , colour = Color.rgb 0 0 0
    }



-- UPDATE


type Msg
    = ColorPickerMsg ColorPicker.Msg
    | CPHexMsg ColorPicker.Msg


update : Msg -> Model -> Model
update message model =
    case Debug.log "" message of
        ColorPickerMsg msg ->
            let
                ( m, colour ) =
                    ColorPicker.update msg model.colour model.colorPicker
                        |> Debug.log ""
            in
                { model
                    | colorPicker = m
                    , colour = colour |> Maybe.withDefault model.colour
                }

        CPHexMsg msg ->
            case ColorPicker.hex2Color model.hex of
                Just col ->
                    let
                        ( m, colour ) =
                            ColorPicker.update msg model.colour model.colorPicker
                    in
                        { model
                            | colorPickerHex = m
                            , hex = colour |> Maybe.map ColorPicker.color2Hex |> Maybe.withDefault model.hex
                        }

                Nothing ->
                    let
                        _ =
                            Debug.log "failed to convert" model.colour
                    in
                        model



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ viewAsColor model

        -- [ viewAsHex model
        ]


viewAsHex : Model -> Html Msg
viewAsHex model =
    div []
        [ h1 [] [ text "Colour Picker - state as hex" ]
        , case ColorPicker.hex2Color model.hex of
            Just c ->
                ColorPicker.view c model.colorPickerHex |> Html.map CPHexMsg

            Nothing ->
                text <| "ColorPicker.hex2Color could not convert " ++ model.hex
        , div [] [ text model.hex ]
        , div [ sts model.hex ] []
        ]


viewAsColor model =
    let
        hex =
            ColorPicker.color2Hex model.colour
    in
        div []
            [ h1 [] [ text "Colour Picker - state as Color" ]
            , div [] [ ColorPicker.view model.colour model.colorPicker |> Html.map ColorPickerMsg ]
            , div [] [ text hex ]
            , div [ sts hex ] []
            ]


sts hex =
    style
        [ ( "width", "50px" )
        , ( "height", "50px" )
        , ( "background-color", hex )
        ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = init
        , update = update
        , view = view
        }
