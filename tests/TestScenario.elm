module TestScenario exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import Model
import Math
import TestCurrency


emptyScenario : Model.Scenario
emptyScenario =
    { months = 12
    , revenue = 30
    , customerGrowth =
        { startValue = 0
        , growthRate = 1
        , churnRate = 0.5
        }
    , revenueGrossMargin = 0.75
    , cac = 50
    , fixedCost = 0
    , comment = Nothing
    , name = Nothing
    }


constantScenario : Model.Scenario
constantScenario =
    { months = 12
    , revenue = 30
    , customerGrowth =
        { startValue = 10
        , growthRate = 0
        , churnRate = 0
        }
    , revenueGrossMargin = 1
    , cac = 50
    , fixedCost = 100
    , comment = Nothing
    , name = Nothing
    }


constantLossyScenario : Model.Scenario
constantLossyScenario =
    { months = 12
    , revenue = 30
    , customerGrowth =
        { startValue = 10
        , growthRate = 1
        , churnRate = 1
        }
    , revenueGrossMargin = 1
    , cac = 50
    , fixedCost = 100
    , comment = Nothing
    , name = Nothing
    }


math : Test
math =
    describe "Math"
        [ describe "effectiveGrowth"
            [ test "1.5 effectiveGrowth for emptyScenario" <|
                \() ->
                    Expect.equal 1.5 <| Math.effectiveGrowth emptyScenario.customerGrowth
            , test "1 effectiveGrowth for constantScenario" <|
                \() ->
                    Expect.equal 1 <| Math.effectiveGrowth constantScenario.customerGrowth
            ]
        , describe "customerAcquisitionCost"
            [ test "0 for constantScenario" <|
                \() ->
                    Expect.equal 0 <| Math.customerAcquisitionCost constantScenario constantScenario.months
            , test "10 * 50 for constantLossyScenario" <|
                \() ->
                    Expect.equal 500 <| Math.customerAcquisitionCost constantLossyScenario constantLossyScenario.months
            ]
        , describe "earnings"
            [ describe "emptyScenario"
                [ test "125 earnings at start" <|
                    \() ->
                        Expect.equal 0 <|
                            Math.earnings emptyScenario Math.firstMonth
                , test "125 earnings at end" <|
                    \() ->
                        Expect.equal 0 <|
                            Math.earnings emptyScenario emptyScenario.months
                ]
            , describe "constantScenario"
                [ test "200 earnings at start" <|
                    \() ->
                        Expect.equal 200 <|
                            Math.earnings constantScenario Math.firstMonth
                , test "200 earnings at end" <|
                    \() ->
                        Expect.equal 200 <|
                            Math.earnings constantScenario constantScenario.months
                ]
            ]
        , describe "cumulativeEarnings"
            [ describe "constantScenario"
                [ test "200 revenue at start" <|
                    \() ->
                        Expect.equal 200 <|
                            Math.cumulativeEarnings constantScenario Math.firstMonth
                , test "200 * 2 revenue at month 2" <|
                    \() ->
                        Expect.equal (200 * 2) <|
                            Math.cumulativeEarnings constantScenario <|
                                Math.firstMonth
                                    + 1
                , test "200 * months revenue at end" <|
                    \() ->
                        Expect.equal (200 * constantScenario.months) <|
                            Math.cumulativeEarnings constantScenario constantScenario.months
                ]
            ]
        , describe "revenue"
            [ describe "constantScenario"
                [ test "300 revenue at start" <|
                    \() ->
                        Expect.equal 300 <|
                            Math.revenue constantScenario Math.firstMonth
                , test "300 revenue at month 1" <|
                    \() ->
                        Expect.equal 300 <|
                            Math.revenue constantScenario 1
                , test "300 revenue at end" <|
                    \() ->
                        Expect.equal 300 <|
                            Math.revenue constantScenario constantScenario.months
                ]
            ]
        , describe "customers"
            [ describe "constantScenario"
                [ test "10 customers at start" <|
                    \() ->
                        Expect.equal 10 <|
                            Math.customers constantScenario.customerGrowth Math.firstMonth
                , test "10 customers at end" <|
                    \() ->
                        Expect.equal 10 <|
                            Math.customers constantScenario.customerGrowth
                                constantScenario.months
                ]
            , describe "emptyScenario"
                [ test "0 customers at start" <|
                    \() ->
                        Expect.equal 0 <|
                            Math.customers emptyScenario.customerGrowth Math.firstMonth
                , test "0 customers at end" <|
                    \() ->
                        Expect.equal 0 <|
                            Math.customers emptyScenario.customerGrowth emptyScenario.months
                ]
            ]
        ]


all : Test
all =
    describe "All tests" [ math, TestCurrency.currency ]
