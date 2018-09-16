module ColorPickerTests exposing (color2HexTests, hexStateTests)

import Color
import ColorPicker exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)


blackHex =
    "#000000"


black2 =
    { alpha = 1
    , hue = pi
    , saturation = 0
    , lightness = 0
    }


hexStateTests =
    describe "hexState tests"
        [ test "convert black" <|
            \() ->
                ColorPicker.hex2Color blackHex
                    |> Debug.log ""
                    |> Expect.equal (Just Color.black)
        , test "safeToHsl black" <|
            \() ->
                ColorPicker.hex2Color blackHex
                    |> Maybe.map (safeToHsl pi)
                    |> Expect.equal (Just black2)
        ]



-- handleSliderClickTests =
--     describe "handleSliderClick Tests"
--         [ test "switch to reds" <|
--             \_ ->
--                 handleSliderClick x model
--                     |> Tuple.second
--                     |> Expect.equal Color.red
--         ]
-- hex2hexTests : Test
-- hex2hexTests =
--     let
--         mkTest ( _, _, _, hx ) =
--             test hx <|
--                 \_ ->
--                     Expect.equal (hx |> hex2Color |> Maybe.map color2Hex) (Just hx)
--     in
--         testItems
--             |> List.map mkTest
--             |> describe "hex2Color Tests"
--
--


color2HexTests =
    describe "color2Hex Tests"
        [ test "red" <|
            \_ ->
                color2Hex Color.red
                    |> Expect.equal "#0d0000"
        ]



-- hsl2hslTests : Test
-- hsl2hslTests =
--     let
--         mkTest { hh, ss, ll, hex } =
--             test hex <|
--                 \_ ->
--                     let
--                         col =
--                             Color.hsl (hh / 360 * 2 * pi) (ss / 100) (ll / 100)
--
--                         col_ =
--                             let
--                                 { red, green, blue } =
--                                     Color.toRgba col
--                             in
--                             Color.rgb red green blue
--                     in
--                     Expect.equal (col |> color2Hex |> hex2Color) (Just col_)
--     in
--     testItems
--         |> List.map mkTest
--         |> describe "hex2hex Tests"
--
--


type alias HSLHex =
    { hh : Float
    , ss : Float
    , ll : Float
    , hex : String
    }


testItems : List HSLHex
testItems =
    [ HSLHex 195 100 50 "#00bfff"
    , HSLHex 0 0 100 "#ffffff"
    , HSLHex 0 0 0 "#000000"
    , HSLHex 0 100 100 "#ff0000"
    , HSLHex 120 100 100 "#00ff00"
    , HSLHex 240 100 100 "#0000ff"
    , HSLHex 60 100 100 "#ffff00"
    , HSLHex 180 100 100 "#00ffff"
    , HSLHex 300 100 100 "#ff00ff"
    , HSLHex 0 0 50 "#808080"
    , HSLHex 0 100 50 "#800000"
    , HSLHex 60 100 50 "#808000"
    , HSLHex 120 100 50 "#008000"
    , HSLHex 300 100 50 "#800080"
    , HSLHex 180 100 50 "#008080"
    , HSLHex 240 100 50 "#000080"
    ]
