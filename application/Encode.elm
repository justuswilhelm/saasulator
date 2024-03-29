module Encode exposing (..)

import Model exposing (..)
import Json.Encode exposing (..)
import Dict


encodeScenarios : Scenarios -> Value
encodeScenarios scenarios =
    object <| List.map (\( k, v ) -> ( toString k, encodeScenario v )) <| Dict.toList scenarios


nullableString : Maybe String -> Value
nullableString =
    Maybe.map string >> Maybe.withDefault null


encodeScenario : Scenario -> Value
encodeScenario scenario =
    object
        [ ( "months", int scenario.months )
        , ( "revenue", int scenario.revenue )
        , ( "customerGrowth", encodeCustomerGrowth scenario.customerGrowth )
        , ( "revenueGrossMargin", float scenario.revenueGrossMargin )
        , ( "cac", int scenario.cac )
        , ( "fixedCost", int scenario.fixedCost )
        , ( "comment", nullableString scenario.comment )
        , ( "name", nullableString scenario.name )
        ]


encodeCustomerGrowth : CustomerGrowth -> Value
encodeCustomerGrowth customerGrowth =
    object
        [ ( "start", int customerGrowth.startValue )
        , ( "growth", float customerGrowth.growthRate )
        , ( "churn", float customerGrowth.churnRate )
        ]
