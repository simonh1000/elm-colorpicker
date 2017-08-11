module ColorPicker exposing (State, Msg, empty, update, view, color2Hex, hex2Color)

{-| An Elm library to help you implement a color picker tool.

@docs State, Msg, empty, update, view, color2Hex, hex2Color

-}

import Html exposing (..)
import Html.Attributes as Attr
import Json.Decode as Json exposing (..)
import Color exposing (Color)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Hex


{-| Opaque type. Needs to be added to your model. You will also need to store a color in your model

    type alias Model =
        { myColour : Color
        , colorPicker : ColorPicker.Model
        }

-}
type State
    = State Model


type alias Model =
    { pickerMouseDown : Bool
    , sliderMouseDown : Bool
    }


{-| Initial ColorPicker state

    init =
        { myColour = Color.red
        , colorPicker = ColorPicker.empty
        }

-}
empty : State
empty =
    State
        { pickerMouseDown = False
        , sliderMouseDown = False
        }


{-| Opaque type. These messages are handled by `ColorPicker.update`
-}
type Msg
    = PickerClick ( Int, Int )
    | PickerMouseDown Bool
    | SliderClick ( Int, Int )
    | SliderMouseDown Bool


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
    case message of
        PickerClick ( x, y ) ->
            let
                { hue, saturation, lightness, alpha } =
                    Color.toHsl col

                newColour =
                    Color.hsl hue (toFloat x / 200) (1 - toFloat y / 150)
            in
                ( State model, Just newColour )

        PickerMouseDown val ->
            ( State { model | pickerMouseDown = val }, Nothing )

        SliderClick ( x, _ ) ->
            let
                { hue, saturation, lightness } =
                    Color.toHsl col

                newColour =
                    Color.hsl (toFloat x / 200 * 2 * pi) saturation lightness
            in
                ( State model, Just newColour )

        SliderMouseDown val ->
            ( State { model | sliderMouseDown = val }, Nothing )


{-| Renders the color picker on screen
-}
view : Color -> State -> Html Msg
view col (State model) =
    div
        [ Attr.id "color-picker"
        , Attr.style
            [ ( "background-color", "white" )
            , ( "padding", "2px" )
            , ( "display", "inline-block" )
            ]
        ]
        [ div [ pickerStyles ] [ picker col model, pickerIndicator col model ]
        , div [ pickerStyles ] [ slider model, sliderIndicator col model ]
        ]


picker : Color -> Model -> Svg Msg
picker col model =
    let
        { hue, saturation, lightness } =
            Color.toHsl col

        colHex =
            color2Hex <| Color.hsl hue 1 0.5

        cx_ =
            saturation * 200 |> round |> toString

        cy_ =
            150 - lightness * 150 |> round |> toString
    in
        svg
            [ width "200", height "150" ]
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
            , rect [ id "picker", width "200", height "150", fill colHex ] []
            , rect [ width "200", height "150", fill "url(#pickerSaturation)" ] []
            , rect
                ([ width "200"
                 , height "150"
                 , fill "url(#pickerBrightness)"
                 ]
                    ++ dragAttrs model.pickerMouseDown PickerMouseDown PickerClick
                )
                []
            ]


pickerIndicator : Color -> Model -> Html Msg
pickerIndicator col model =
    let
        { hue, saturation, lightness } =
            Color.toHsl col

        cx_ =
            saturation * 200 - 3 |> round |> toString

        cy_ =
            150 - lightness * 150 - 2 |> round |> toString
    in
        div
            [ Attr.style
                [ ( "position", "absolute" )
                , ( "top", cy_ ++ "px" )
                , ( "left", cx_ ++ "px" )
                , ( "border-radius", "100%" )
                , ( "border", "2px solid white" )
                , ( "width", "6px" )
                , ( "height", "6px" )
                , ( "pointer-events", "none" )
                ]
            ]
            []


slider : Model -> Svg Msg
slider { sliderMouseDown } =
    let
        ss os sc =
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
            [ width "200", height "20" ]
            [ defs []
                [ linearGradient
                    [ id "gradient-hsv", x1 "100%", y1 "0%", x2 "0%", y2 "0%" ]
                    (stops |> List.map (uncurry ss))
                ]
            , rect
                ([ x "0"
                 , y "0"
                 , width "100%"
                 , height "100%"
                 , fill "url(#gradient-hsv)"
                 ]
                    ++ dragAttrs sliderMouseDown SliderMouseDown SliderClick
                )
                []
            ]


sliderIndicator : Color -> Model -> Html Msg
sliderIndicator col model =
    let
        { hue, saturation, lightness } =
            Color.toHsl col

        xVal =
            -- shift by 4px to center on selected color
            (hue / 2 / pi * 200 - 4) |> round |> toString
    in
        div
            [ Attr.style
                [ ( "position", "absolute" )
                , ( "top", "-3px" )
                , ( "left", xVal ++ "px" )
                , ( "border", "3px solid #ddd" )
                , ( "height", "26px" )
                , ( "width", "9px" )
                , ( "pointer-events", "none" )
                ]
            ]
            []


dragAttrs : Bool -> (Bool -> Msg) -> (( Int, Int ) -> Msg) -> List (Svg.Attribute Msg)
dragAttrs mouseDown mdMsg clMsg =
    let
        common =
            [ onMouseDown (mdMsg True)
            , onMouseUp (mdMsg False)
            , onClickSvg clMsg
            ]
    in
        if mouseDown then
            onMouseMovePos clMsg :: common
        else
            onMouseOut (mdMsg False) :: common


{-| Converts `Color` to `String` (with preceding `#`).
Used internally and exposed because the public alternative is a library with multiple dependencies.
-}
color2Hex : Color -> String
color2Hex col =
    let
        { red, green, blue } =
            Color.toRgb col
    in
        [ red, green, blue ]
            |> List.map padHex
            |> String.join ""
            |> (++) "#"


{-| Converts `Color` to `String` (with preceding `#`).
Used internally and exposed because the public alternative is a library with multiple dependencies.
-}
hex2Color : String -> Maybe Color
hex2Color s =
    let
        hex =
            String.toLower s

        conv begin end =
            String.slice begin end >> Hex.fromString
    in
        case ( conv 1 3 hex, conv 3 5 hex, conv 5 7 hex ) of
            ( Ok rr, Ok gg, Ok bb ) ->
                Just <| Color.rgb rr gg bb

            _ ->
                Nothing



-- Styles


pickerStyles : Html.Attribute msg
pickerStyles =
    Attr.style
        [ ( "cursor", "crosshair" )
        , ( "position", "relative" )
        ]



-- Helpers


padHex : Int -> String
padHex x =
    if x < 16 then
        "0" ++ Hex.toString x
    else
        Hex.toString x


onClickSvg : (( Int, Int ) -> Msg) -> Svg.Attribute Msg
onClickSvg msgCreator =
    on "click" (Json.map msgCreator decodePoint)


onMouseMovePos : (( Int, Int ) -> Msg) -> Svg.Attribute Msg
onMouseMovePos msgCreator =
    on "mousemove" (Json.map msgCreator decodePoint)


decodePoint : Decoder ( Int, Int )
decodePoint =
    map2 (,)
        (field "offsetX" Json.int)
        (field "offsetY" Json.int)
