module PhotoGrooveTests exposing (..)

import Expect
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as Decode exposing (decodeValue)
import Json.Encode as Encode
import PhotoGroove exposing (Model, Msg(..), initialModel, update)
import Test exposing (Test, describe, fuzz, fuzz2, test)


suite : Test
suite =
    test "One plus one equal two" <|
        \_ ->
            Expect.equal 2 (1 + 1)


decoderTest : Test
decoderTest =
    fuzz2 string int "title defaults to (untitled)" <|
        \url size ->
            [ ( "url", Encode.string url )
            , ( "size", Encode.int size )
            ]
                |> Encode.object
                |> decodeValue PhotoGroove.photoDecoder
                |> Result.map .title
                |> Expect.equal
                    (Ok "(untitled)")


slidHueSetsHue : Test
slidHueSetsHue =
    fuzz int "SlidHue sets the hue" <|
        \amount ->
            initialModel
                |> update (SlidHue amount)
                |> Tuple.first
                |> .hue
                |> Expect.equal amount


sliders : Test
sliders =
    describe "Slider sets the desired field in the Model"
        [ testSlider "SlidHue" SlidHue .hue
        , testSlider "SlidRipple" SlidRipple .ripple
        , testSlider "SlidNoise" SlidNoise .noise
        ]


testSlider : String -> (Int -> Msg) -> (Model -> Int) -> Test
testSlider description toMsg amountFromModel =
    fuzz int (description ++ " sets the hue") <|
        \amount ->
            initialModel
                |> update (toMsg amount)
                |> Tuple.first
                |> amountFromModel
                |> Expect.equal amount
