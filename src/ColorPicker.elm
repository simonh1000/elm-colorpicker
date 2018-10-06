module ColorPicker exposing (State, Msg, empty, update, view, color2Hex, hex2Color)

{-| An Elm library to help you implement a color picker tool.

@docs State, Msg, empty, update, view, color2Hex, hex2Color

The main picker is for saturation and lightness, while the sliders below are for hie and opacity respectively.

-}

import Color exposing (Color)
import Hex
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events
import Json.Decode as Decode exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)


widgetWidth =
    200


{-| Opaque type. Needs to be added to your model. You will also need to store a `Color` in your model

    type alias Model =
        { myColour : Color
        , colorPicker : ColorPicker.Model
        }

-}
type State
    = State Model


{-| Initial ColorPicker state

    init =
        { myColour = Color.red
        , colorPicker = ColorPicker.empty
        }

-}
empty : State
empty =
    State blankModel


{-| The model stores the hue because dark colours has indistinguihsable hues.
But the user could easily change their hex between updates so we need to check that this has _probably_
not happened.
-}
type alias Model =
    { mouseTarget : MouseTarget
    , hue : Float -- 0.1 .. 1.0
    }


blankModel : Model
blankModel =
    { mouseTarget = Unpressed
    , hue = 0.5
    }


type MouseTarget
    = Unpressed
    | SatLight
    | HueSlider
    | OpacitySlider



-- ------------------------------
-- U P D A T E
-- ------------------------------


{-| Opaque type. These messages are handled by `ColorPicker.update`
-}
type Msg
    = SetMouseTarget MouseTarget
    | OnMouseMove MouseTarget MouseInfo
    | OnClick MouseTarget MouseInfo
    | NoOp


{-| On each update, ColorPicker returns its model and (where appropriate) the new selected colo(u)r.

    ColorPickermsg msg ->
        let
            (cp, col) =
                ColorPicker.update msg model.colorPicker
        in
            { model | colorPicker = cp, myColor = Maybe.withDefault model.myColor col }

-}
update : Msg -> Color -> State -> ( State, Maybe Color )
update message col (State model) =
    update_ message col model
        |> Tuple.mapFirst State


update_ : Msg -> Color -> Model -> ( Model, Maybe Color )
update_ message col model =
    let
        handleMouseMove mouseTarget mouseInfo =
            if mouseInfo.mousePressed && model.mouseTarget == mouseTarget then
                ( model, Just <| calcNewColour mouseTarget col model.hue mouseInfo )

            else if not mouseInfo.mousePressed && model.mouseTarget == mouseTarget then
                ( { model | mouseTarget = Unpressed }, Nothing )

            else
                ( model, Nothing )

        calcNewColour mouseTarget =
            case mouseTarget of
                SatLight ->
                    calcSatLight

                HueSlider ->
                    calcHue

                OpacitySlider ->
                    calcOpacity

                Unpressed ->
                    \_ _ _ -> col
    in
    case message of
        SetMouseTarget mouseTarget ->
            ( { model | mouseTarget = mouseTarget }, Nothing )

        OnMouseMove mouseTarget mouseInfo ->
            handleMouseMove mouseTarget mouseInfo

        OnClick mouseTarget mouseInfo ->
            let
                m =
                    if mouseTarget == HueSlider then
                        setHue mouseInfo model

                    else
                        model
            in
            ( m, Just <| calcNewColour mouseTarget col model.hue mouseInfo )

        NoOp ->
            ( model, Nothing )


setHue : MouseInfo -> Model -> Model
setHue mouseInfo model =
    { model | hue = toFloat mouseInfo.x / widgetWidth }


calcSatLight : Color -> Float -> MouseInfo -> Color
calcSatLight col currHue { x, y, mousePressed } =
    let
        hsla =
            Color.toHsla col
    in
    { hsla
        | hue = currHue
        , saturation = toFloat x / widgetWidth
        , lightness = 1 - toFloat y / 150
    }
        |> Color.fromHsla


calcHue : Color -> Float -> MouseInfo -> Color
calcHue col currHue { x, mousePressed } =
    let
        { saturation, lightness, alpha } =
            safeToHsl currHue col

        hue =
            toFloat x / widgetWidth
    in
    -- Enable 'escape from black'
    if saturation == 0 && lightness < 0.02 then
        Color.hsla hue 0.5 0.5 alpha

    else
        Color.hsla hue saturation lightness alpha


calcOpacity : Color -> Float -> MouseInfo -> Color
calcOpacity col _ { x, mousePressed } =
    let
        hsla =
            Color.toHsla col
    in
    { hsla | alpha = toFloat x / widgetWidth }
        |> Color.fromHsla



-- ------------------------------
-- V I E W
-- ------------------------------


{-| Renders the color picker on screen
-}
view : Color -> State -> Html Msg
view col (State model) =
    let
        colCss =
            Color.hsl model.hue 1 0.5
                |> Color.toCssString

        { hue, alpha } =
            Color.toHsla col
    in
    div
        [ Attr.style "background-color" "white"
        , Attr.style "padding" "6px"
        , Attr.style "display" "inline-block"
        , Attr.style "border-radius" "5px"
        , Attr.style "box-shadow" "rgba(0, 0, 0, 0.15) 0px 0px 0px 1px, rgba(0, 0, 0, 0.15) 0px 8px 16px"
        , Attr.class "color-picker-container"
        , bubblePreventer
        ]
        [ div pickerStyles
            [ satLightPalette colCss model
            , pickerIndicator model.hue col
            ]
        , div (pickerStyles ++ sliderContainerStyles "hue")
            [ huePalette model
            , hueMarker model.hue col
            ]
        , div (checkedBkgStyles ++ pickerStyles ++ sliderContainerStyles "opacity")
            [ opacityPalette colCss model
            , alphaMarker alpha
            ]
        ]


satLightPalette : String -> Model -> Svg Msg
satLightPalette colCss model =
    svg
        [ width (String.fromInt widgetWidth)
        , height "150"
        , class "main-picker"
        , display "block"
        ]
        [ defs
            []
            [ linearGradient
                [ id "pickerSaturation" ]
                [ stop [ offset "0", stopColor "#808080", stopOpacity "1" ] []
                , stop [ offset "1", stopColor "#808080", stopOpacity "0" ] []
                ]
            , linearGradient
                [ id "pickerBrightness", x1 "0", y1 "0", x2 "0", y2 "1" ]
                [ stop [ offset "0", stopColor "#fff", stopOpacity "1" ] []
                , stop [ offset "0.499", stopColor "#fff", stopOpacity "0" ] []
                , stop [ offset "0.5", stopColor "#000", stopOpacity "0" ] []
                , stop [ offset "1", stopColor "#000", stopOpacity "1" ] []
                ]
            ]
        , rect [ width (String.fromInt widgetWidth), height "150", fill colCss, id "picker" ] []
        , rect [ width (String.fromInt widgetWidth), height "150", fill "url(#pickerSaturation)" ] []
        , rect
            ([ width (String.fromInt widgetWidth)
             , height "150"
             , fill "url(#pickerBrightness)"
             ]
                ++ dragAttrs model.mouseTarget SatLight (OnMouseMove SatLight)
            )
            []
        ]


{-| pick saturation & lightness
-}
pickerIndicator : Float -> Color -> Html Msg
pickerIndicator hue col =
    let
        { saturation, lightness } =
            safeToHsl hue col

        borderColor =
            if lightness > 0.95 then
                "#cccccc"

            else
                "#ffffff"

        cx_ =
            saturation * widgetWidth - 3 |> round |> String.fromInt

        cy_ =
            150 - lightness * 150 - 3 |> round |> String.fromInt
    in
    div
        [ Attr.style "position" "absolute"
        , Attr.style "top" (cy_ ++ "px")
        , Attr.style "left" (cx_ ++ "px")
        , Attr.style "border-radius" "100%"
        , Attr.style "border" ("2px solid " ++ borderColor)
        , Attr.style "width" "6px"
        , Attr.style "height" "6px"
        , Attr.style "pointer-events" "none"
        ]
        []



-- --------------------------
-- HUE
-- --------------------------


huePalette : Model -> Svg Msg
huePalette model =
    let
        mkStop ( os, sc ) =
            stop [ offset os, stopColor sc, stopOpacity "1" ] []

        stops : List ( String, String )
        stops =
            [ ( "0%", "#FF0000" )
            , ( "17%", "#FF00FF" )
            , ( "33%", "#0000FF" )
            , ( "50%", "#00FFFF" )
            , ( "66%", "#00FF00" )
            , ( "83%", "#FFFF00" )
            , ( "100%", "#FF0000" )
            ]
    in
    svg
        (class "hue-picker" :: sliderStyles)
        [ defs []
            [ linearGradient
                [ id "gradient-hsv", x1 "100%", y1 "0%", x2 "0%", y2 "0%" ]
                (stops |> List.map mkStop)
            ]
        , rect
            ([ x "0"
             , y "0"
             , width (String.fromInt widgetWidth)
             , height "100%"
             , fill "url(#gradient-hsv)"
             ]
                ++ dragAttrs model.mouseTarget HueSlider (OnMouseMove HueSlider)
            )
            []
        ]



-- --------------------------
-- OPACITY
-- --------------------------


opacityPalette : String -> Model -> Svg Msg
opacityPalette colCss model =
    let
        mkStop ( os, sc, op ) =
            stop [ offset os, stopColor sc, stopOpacity op ] []

        stops : List ( String, String, String )
        stops =
            [ ( "0%", colCss, "1" )
            , ( "100%", colCss, "0" )
            ]
    in
    svg sliderStyles
        [ defs []
            [ linearGradient
                [ id "gradient-opacity", x1 "100%", y1 "0%", x2 "0%", y2 "0%" ]
                (stops |> List.map mkStop)
            ]
        , rect
            ([ x "0"
             , y "0"
             , width (String.fromInt widgetWidth)
             , height "100%"
             , fill "url(#gradient-opacity)"
             ]
                ++ dragAttrs model.mouseTarget OpacitySlider (OnMouseMove OpacitySlider)
            )
            []
        ]


{-| Select the hue
-}
hueMarker : Float -> Color -> Html Msg
hueMarker lastHue col =
    let
        { hue } =
            safeToHsl lastHue col

        correction =
            4

        xVal =
            -- shift by 4px to center on selected color
            (hue * widgetWidth - correction) |> round |> String.fromInt
    in
    div (Attr.style "left" (xVal ++ "px") :: markerAttrs) []


alphaMarker : Float -> Html Msg
alphaMarker alpha =
    let
        correction =
            4

        xVal =
            -- shift by 4px to center on selected color
            (alpha * widgetWidth - correction) |> round |> String.fromInt
    in
    div (Attr.style "left" (xVal ++ "px") :: markerAttrs) []



-- --------------------------
-- Event handlers
-- --------------------------


dragAttrs : MouseTarget -> MouseTarget -> (MouseInfo -> Msg) -> List (Svg.Attribute Msg)
dragAttrs mouseTarget thisTgt onMoveMsg =
    let
        common =
            [ onMouseDown <| SetMouseTarget thisTgt
            , onMouseUp <| SetMouseTarget Unpressed
            , onClickSvg <| OnClick thisTgt
            ]
    in
    if mouseTarget == thisTgt then
        onMouseMovePos onMoveMsg :: common

    else
        common


{-| Hack to prevent SVG click events bubble through to rest of app. SVG does not have an onWithOptions
-}
bubblePreventer : Html.Attribute Msg
bubblePreventer =
    -- stopPropagationOn "click" <| Decode.fail "dont do more"
    stopPropagationOn "click" <| Decode.succeed ( NoOp, True )


onClickSvg : (MouseInfo -> Msg) -> Svg.Attribute Msg
onClickSvg msgCreator =
    on "click" (Decode.map msgCreator decodePoint)


onMouseMovePos : (MouseInfo -> Msg) -> Svg.Attribute Msg
onMouseMovePos msgCreator =
    on "mousemove" (Decode.map msgCreator decodePoint)


type alias MouseInfo =
    { x : Int
    , y : Int
    , mousePressed : Bool
    }


decodePoint : Decoder MouseInfo
decodePoint =
    map3 MouseInfo
        (field "offsetX" Decode.int)
        (field "offsetY" Decode.int)
        (field "buttons" Decode.int |> Decode.map ((/=) 0))



-- Styles


sliderContainerStyles name =
    [ Attr.style "width" (String.fromInt widgetWidth ++ "px")
    , Attr.style "height" "12px"
    , Attr.style "marginTop" "8px"
    , Attr.class <| "color-picker-slider " ++ name
    ]


checkedBkgStyles =
    [ Attr.style "background-size" "12px 12px"
    , Attr.style "background-position" "0 0, 0 6px, 6px -6px, -6px 0px"
    , Attr.style "background-image" "linear-gradient(45deg, #808080 25%, transparent 25%), linear-gradient(-45deg, #808080 25%, transparent 25%), linear-gradient(45deg, transparent 75%, #808080 75%), linear-gradient(-45deg, transparent 75%, #808080 75%)"
    ]


sliderStyles =
    [ width (String.fromInt widgetWidth)
    , height "100%"
    , display "block"
    ]


pickerStyles : List (Html.Attribute msg)
pickerStyles =
    [ Attr.style "cursor" "crosshair"
    , Attr.style "position" "relative"
    ]


markerAttrs =
    [ Attr.style "position" "absolute"
    , Attr.style "top" "1px"
    , Attr.style "bottom" "1px"
    , Attr.style "border" "1px solid #ddd"
    , Attr.style "background-color" "#ffffff"

    -- , Attr.style "height" "10px"
    , Attr.style "width" "6px"

    -- this is essental to enable dragging
    , Attr.style "pointer-events" "none"
    ]



-- Colour Helpers


{-| Converts `Color` to `String` (with preceding `#`).
Used internally and exposed because the public alternative is a library with multiple dependencies.
-}
color2Hex : Color -> String
color2Hex col =
    let
        { red, green, blue } =
            Color.toRgba col
    in
    [ red, green, blue ]
        |> List.map ((*) 255 >> round >> padHex)
        |> String.join ""
        |> (++) "#"


{-| Converts `String` to `Color`.
Used internally and exposed because the public alternative is a library with multiple dependencies.
-}
hex2Color : String -> Maybe Color
hex2Color s =
    let
        hex =
            String.toLower s

        conv begin end =
            String.slice begin end >> Hex.fromString >> Result.map toFloat
    in
    if String.length s /= 7 then
        Nothing

    else
        Result.map3 Color.rgb (conv 1 3 hex) (conv 3 5 hex) (conv 5 7 hex)
            |> Result.toMaybe


{-| Converts colour to
-}
safeToHsl : Float -> Color -> { hue : Float, saturation : Float, lightness : Float, alpha : Float }
safeToHsl lastHue col =
    let
        hsl =
            Color.toHsla col
    in
    { hsl | hue = lastHue }


padHex : Int -> String
padHex x =
    if x < 16 then
        "0" ++ Hex.toString x

    else
        Hex.toString x
