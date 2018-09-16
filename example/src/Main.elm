module Main exposing (main)

import Browser
import Color exposing (Color)
import ColorPicker
import Html exposing (..)
import Html.Attributes exposing (..)


type alias Model =
    { colour : Color
    , colorPicker : ColorPicker.State
    , colorPickerHex : ColorPicker.State
    , hex : String
    }


init : Model
init =
    { colour = Color.rgb 0.5 0.5 0.5
    , colorPicker = ColorPicker.empty
    , hex = "#3bd5d5"
    , colorPickerHex = ColorPicker.empty
    }



-- UPDATE


type Msg
    = ColorPickerMsg ColorPicker.Msg
    | CPHexMsg ColorPicker.Msg


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



-- TODO add an example of a text field using onChange and Html.Keyed


{-| Using the model where state is stored as a Hex
-}
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
        , div (sts model.hex) []
        ]


{-| Using the model where state is stored as a Color
-}
viewAsColor model =
    let
        hex =
            ColorPicker.color2Hex model.colour
    in
    div []
        [ h1 [] [ text "Colour Picker - state as Color" ]
        , div [] [ ColorPicker.view model.colour model.colorPicker |> Html.map ColorPickerMsg ]
        , div [] [ text hex ]
        , div (sts hex) []
        ]


sts hex =
    [ style "width" "50px"
    , style "height" "50px"
    , style "background-color" hex
    ]



--


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
