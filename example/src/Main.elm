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
    , colCss : String
    }


initColour =
    Color.green


init : Model
init =
    { colour = initColour
    , colorPicker = ColorPicker.init <| Just initColour
    , colorPickerHex = ColorPicker.init <| Just initColour
    , hex = ""
    , colCss = ""
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
                        , colCss = colour |> Maybe.map Color.toCssString |> Maybe.withDefault model.hex
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


{-| Using the model where state is stored as a Color
-}
viewAsColor model =
    div []
        [ h1 [] [ text "Colour Picker - state as Color" ]
        , div [] [ ColorPicker.view model.colour model.colorPicker |> Html.map ColorPickerMsg ]
        , div [] [ text <| viewCol model.colour ]
        , div (sts <| Color.toCssString model.colour) []
        ]


viewCol col =
    let
        res =
            Color.toHsla col
    in
    Debug.toString <|
        { res
            | hue = dec 3 res.hue
            , saturation = dec 3 res.saturation
            , lightness = dec 3 res.lightness
        }


dec len f =
    let
        exp =
            10 ^ len
    in
    (f * exp)
        |> round
        |> toFloat
        |> (\f_ -> f_ / exp)


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
        , div [] [ text model.colCss ]
        , div [] [ text model.colCss ]
        , div (sts model.colCss) []
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
