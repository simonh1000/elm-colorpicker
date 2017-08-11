module Example exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Color
import ColorPicker exposing (..)


hex2hexTests : Test
hex2hexTests =
    let
        mkTest ( _, _, _, hx ) =
            test hx <|
                \_ ->
                    Expect.equal (hx |> hex2Color |> Maybe.map color2Hex) (Just hx)
    in
        testItems
            |> List.map mkTest
            |> describe "hex2Color Tests"


hsl2hslTests : Test
hsl2hslTests =
    let
        mkTest ( hh, ss, ll, hx ) =
            test hx <|
                \_ ->
                    let
                        col =
                            Color.hsl (hh / 360 * 2 * pi) (ss / 100) (ll / 100)

                        col_ =
                            let
                                { red, green, blue } =
                                    Color.toRgb col
                            in
                                Color.rgb red green blue
                    in
                        Expect.equal (col |> color2Hex |> hex2Color) (Just col_)
    in
        testItems
            |> List.map mkTest
            |> describe "hex2hex Tests"


testItems : List ( number, number1, number2, String )
testItems =
    [ ( 195, 100, 50, "#00bfff" )
    , ( 0, 0, 100, "#ffffff" )
    , ( 0, 0, 0, "#000000" )
    , ( 0, 100, 100, "#ff0000" )
    , ( 120, 100, 100, "#00ff00" )
    , ( 240, 100, 100, "#0000ff" )
    , ( 60, 100, 100, "#ffff00" )
    , ( 180, 100, 100, "#00ffff" )
    , ( 300, 100, 100, "#ff00ff" )
    , ( 0, 0, 50, "#808080" )
    , ( 0, 100, 50, "#800000" )
    , ( 60, 100, 50, "#808000" )
    , ( 120, 100, 50, "#008000" )
    , ( 300, 100, 50, "#800080" )
    , ( 180, 100, 50, "#008080" )
    , ( 240, 100, 50, "#000080" )
    ]



-- fuzH : Fuzzer Float
-- fuzH =
--     Fuzz.map toFloat <| intRange 0 255
--
--
-- round100 : Float -> Float
-- round100 =
--     flip (/) 100 << toFloat << round << (*) 100
--
--
-- fuzSV : Fuzzer Float
-- fuzSV =
--     Fuzz.map round100 <| Fuzz.floatRange 0 1
--
--
-- fuzHSV : Fuzzer HSV
-- fuzHSV =
--     Fuzz.map3 HSV fuzH fuzSV fuzSV
-- fuzz : Fuzzer a -> String -> (a -> Expectation) -> Test
-- fuz : Test
-- fuz =
--     fuzz fuzHSV "fuz" <|
--         \hsv -> Expect.equal (rgb2hsv <| hsv2rgb hsv) hsv
--
