# Elm ColorPicker widget

An Elm library to help you implement a color picker tool without relying upon the presence - or [otherwise](http://caniuse.com/#feat=input-color) - of a built in browser widget.

<img src="https://github.com/simonh1000/elm-colorpicker/raw/master/screenshot.png" alt="screen shot">

## Example

```elm
import ColorPicker

type alias Model =
    { colorPicker : ColorPicker.State
    , colour : Color
    }

init : Model
init =
    { colorPicker = ColorPicker.empty
    , colour = Color.rgb 255 0 0
    }

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

view : Model -> Html Msg
view model =
    ColorPicker.view model.myColour model.colorPicker
        |> Html.map ColorPickerMsg
```

## Example

See it in action

```sh
$ cd example
$ npm install
$ npm run dev
Open http://localhost:3000
```

### To Do

 * Enable use of different sizes of svg elements

### Changelog

 * 1.0.0 : Initial release


### Inspired by

 * https://github.com/bendc/color-picker
 * https://github.com/DavidDurman/FlexiColorPicker
 * https://stackoverflow.com/questions/17242144/javascript-convert-hsb-hsv-color-to-rgb-accurately?answertab=votes#tab-top
 * Elm-sortable-table's API
