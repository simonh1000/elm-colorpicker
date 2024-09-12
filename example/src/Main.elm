module Main exposing (main)

import Browser
import Color exposing (Color)
import ColorPicker
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events exposing (onClick)
import Json.Decode as Decode


type alias Model =
    { colour : Color
    , colorPicker : ColorPicker.State
    , --
      colour2 : Color
    , colorPicker2 : ColorPicker.State
    , -- as dropdown
      cp3IsOpen : Bool
    , colour3 : Color
    , colorPicker3 : ColorPicker.State
    }


initColour =
    Color.green


init : Model
init =
    { colour = initColour
    , colorPicker = ColorPicker.empty
    , --
      colour2 = initColour
    , colorPicker2 = ColorPicker.empty
    , cp3IsOpen = False
    , colour3 = initColour
    , colorPicker3 = ColorPicker.empty
    }



-- UPDATE


type Msg
    = ColorPickerMsg ColorPicker.Msg
    | ColorPickerMsg2 ColorPicker.Msg
    | ToggleCpOpen
    | OnBlur
    | ColorPickerMsg3 ColorPicker.Msg


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

        ColorPickerMsg2 msg ->
            let
                ( m, colour ) =
                    ColorPicker.update msg model.colour2 model.colorPicker2
            in
            { model
                | colorPicker2 = m
                , colour2 = colour |> Maybe.withDefault model.colour2
            }

        ToggleCpOpen ->
            { model | cp3IsOpen = not model.cp3IsOpen }

        ColorPickerMsg3 msg ->
            let
                ( m, colour ) =
                    ColorPicker.update msg model.colour3 model.colorPicker3
            in
            { model
                | colorPicker3 = m
                , colour3 = colour |> Maybe.withDefault model.colour3
            }

        OnBlur ->
            { model | cp3IsOpen = False }



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ viewAsColor model

        --, view2 model
        , viewColorWithAttrs model
        ]


viewColorWithAttrs : Model -> Html Msg
viewColorWithAttrs model =
    let
        cssColor =
            Color.toCssString model.colour3

        attrs =
            if model.cp3IsOpen then
                [ Events.on "blur" <| Decode.succeed OnBlur ]

            else
                []
    in
    div [ class "mt-6" ]
        [ div
            (attrs
                ++ [ class "colorpicker show-focus"
                   , tabindex 0
                   ]
            )
            [ div [ class "flex flex-row" ]
                [ viewColorSample ToggleCpOpen cssColor
                , text cssColor
                ]
            , if model.cp3IsOpen then
                viewColorPicker model.colorPicker3 model.colour3
                    |> Html.map ColorPickerMsg3

              else
                text ""
            ]
        ]


viewColorPicker : ColorPicker.State -> Color -> Html ColorPicker.Msg
viewColorPicker state hex =
    div [ class "colorpicker__container" ]
        [ ColorPicker.view hex state
        ]


viewColorSample : msg -> String -> Html msg
viewColorSample toggleMsg hex =
    div
        [ class "color-swatch"
        , style "background-color" hex
        , onClick toggleMsg
        ]
        [ text "v" ]



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


view2 model =
    div []
        [ h1 [] [ text "Colour Picker - state as Color" ]
        , div [] [ ColorPicker.view model.colour2 model.colorPicker2 |> Html.map ColorPickerMsg2 ]
        , div [] [ text <| viewCol model.colour2 ]
        , div (sts <| Color.toCssString model.colour2) []
        ]


viewCol col =
    let
        res =
            Color.toHsla col

        toStr =
            dec 3 >> String.fromFloat
    in
    ("{ alpha = " ++ String.fromFloat res.alpha)
        ++ (", hue = " ++ toStr res.hue)
        ++ (", lightness = " ++ toStr res.lightness)
        ++ (", saturation = " ++ toStr res.saturation ++ " }")


dec : Float -> Float -> Float
dec len f =
    let
        exp =
            10 ^ len
    in
    (f * exp)
        |> round
        |> toFloat
        |> (\f_ -> f_ / exp)


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
