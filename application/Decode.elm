module Decode exposing (..)

import Model exposing (..)
import Json.Decode exposing (..)
import Dict


decodeInt : String -> Int
decodeInt string =
    case String.toInt string of
        Ok value ->
            value

        Err _ ->
            0


decodeIntWithMaximum : String -> Int -> Int
decodeIntWithMaximum string value =
    min (decodeInt string) value


decodePercentage : String -> Float
decodePercentage string =
    case String.toFloat string of
        Ok value ->
            value / 100

        _ ->
            0


decodeCurrency : String -> Currency
decodeCurrency string =
    case string of
        "aud" ->
            AUD

        "eur" ->
            EUR

        "jpy" ->
            JPY

        "usd" ->
            USD

        _ ->
            USD


scenario : Decoder Scenario
scenario =
    map7 Scenario
        (field "months" int)
        (field "churnRate" float)
        (field "revenue" int)
        (field "customerGrowth" customerGrowth)
        (field "revenueGrossMargin" float)
        (field "cac" int)
        (field "fixedCost" int)


customerGrowth : Decoder CustomerGrowth
customerGrowth =
    map2 Relative
        (field "start" int)
        (field "growth" float)


scenarios : Decoder (Dict.Dict String Scenario)
scenarios =
    dict scenario


decodeScenarios : Result String (Dict.Dict String Scenario) -> Result String (Dict.Dict Int Scenario)
decodeScenarios result =
    case result of
        Ok raw ->
            raw
                |> Dict.toList
                |> List.map (\( k, v ) -> ( String.toInt k, v ))
                |> List.filter
                    (\( k, _ ) ->
                        case k of
                            Ok _ ->
                                True

                            _ ->
                                False
                    )
                |> List.map
                    (\( k, v ) ->
                        case k of
                            Ok k ->
                                ( k, v )

                            _ ->
                                ( 0, v )
                    )
                |> Dict.fromList
                |> Ok

        Err msg ->
            Err msg