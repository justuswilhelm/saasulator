module Update exposing (update)

import Model exposing (Model, maxMonths, newScenario, currentScenario, ScenarioID)
import Msg exposing (..)
import Dict
import Decode exposing (..)
import Json.Decode
import Cmds exposing (..)
import Currency.Decode exposing (decodeCurrency)


maybeString : String -> Maybe String
maybeString string =
    if string == "" then
        Nothing
    else
        Just string


localStorage : Model -> ( String, Maybe String ) -> ( Model, Cmd Msg )
localStorage model ( key, value ) =
    case key of
        "currency" ->
            case value of
                Just value ->
                    let
                        currency =
                            decodeCurrency value
                                |> Debug.log value
                                |> Maybe.withDefault Model.defaultCurrency
                    in
                        { model | currency = currency } ! []

                Nothing ->
                    model ! [ storeCurrency model.currency ]

        "scenarios" ->
            case value of
                Nothing ->
                    let
                        model_ =
                            { model | scenarios = Model.newScenarios }
                    in
                        { model_
                            | currentScenario = Model.firstScenarioID model_.scenarios
                        }
                            ! [ storeScenarios model_
                              ]

                Just scenarios ->
                    case decodeScenarios <| Json.Decode.decodeString Decode.scenarios scenarios of
                        Ok scenarios ->
                            let
                                model_ =
                                    if scenarios == Dict.empty then
                                        { model | scenarios = Model.newScenarios }
                                    else
                                        { model | scenarios = scenarios }
                            in
                                { model_
                                    | currentScenario = Model.firstScenarioID model_.scenarios
                                }
                                    ! []

                        Err _ ->
                            let
                                model_ =
                                    { model | scenarios = Model.newScenarios }
                            in
                                { model_
                                    | currentScenario = Model.firstScenarioID model_.scenarios
                                }
                                    ! [ storeScenarios model_ ]

        _ ->
            model ! []


setScenario : Model -> ScenarioMsg -> ScenarioID -> String -> ( Model, Cmd Msg )
setScenario model msg scenarioID value =
    let
        setScenario scenario msg value =
            case msg of
                SetMonths ->
                    { scenario
                        | months = decodeIntWithMaximum value maxMonths
                    }

                SetCustomerStart ->
                    Model.setStartValue scenario <| decodeInt value

                SetChurnRate ->
                    Model.setChurn scenario <| decodePercentage value

                SetCustomerGrowth ->
                    Model.updateGrowth scenario <| decodePercentage value

                SetRevenue ->
                    { scenario
                        | revenue = decodeInt value
                    }

                SetCAC ->
                    { scenario
                        | cac = decodeInt value
                    }

                SetMargin ->
                    { scenario
                        | revenueGrossMargin = decodePercentage value
                    }

                SetFixedCost ->
                    { scenario
                        | fixedCost = decodeInt value
                    }

                SetName ->
                    { scenario
                        | name = maybeString value
                    }

                SetComment ->
                    { scenario
                        | comment = maybeString value
                    }
    in
        case Dict.get scenarioID model.scenarios of
            Nothing ->
                model ! []

            Just scenario ->
                let
                    scenarios =
                        Dict.insert scenarioID
                            (setScenario scenario msg value)
                            model.scenarios

                    model_ =
                        { model | scenarios = scenarios }
                in
                    model_ ! [ storeScenarios model_ ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetScenario scenarioID msg value ->
            setScenario model msg scenarioID value

        ChooseScenario id ->
            if Dict.member id model.scenarios then
                { model | currentScenario = Just id } ! []
            else
                model ! []

        NewScenario ->
            let
                highest =
                    Model.lowestFreeId model.scenarios

                scenarios_ =
                    Dict.insert highest Model.newScenario model.scenarios

                model_ =
                    { model
                        | scenarios = scenarios_
                        , currentScenario = Just highest
                    }
            in
                model_
                    ! [ storeScenarios model_ ]

        SetCurrency value ->
            let
                currency =
                    decodeCurrency value |> Maybe.withDefault Model.defaultCurrency
            in
                { model | currency = currency } ! [ storeCurrency currency ]

        LocalStorageReceive kv ->
            localStorage model kv

        DeleteScenario id ->
            let
                scenarios =
                    Dict.remove id model.scenarios

                currentScenario =
                    Model.firstScenarioID scenarios

                model_ =
                    { model
                        | scenarios = scenarios
                        , currentScenario = currentScenario
                    }
            in
                model_ ! [ storeScenarios model_ ]
