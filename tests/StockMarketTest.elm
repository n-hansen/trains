module StockMarketTest exposing (suite)

import Dict
import Expect
import Result
import StockMarket exposing (..)
import Test exposing (..)
import Util.Dict as Dict


suite : Test
suite =
    describe "StockMarket module"
        [ describe "Simple market example" <|
            let
                p1 =
                    "Alice"

                p2 =
                    "Bob"

                p3 =
                    "Candice"

                p4 =
                    "Doug"

                c1 =
                    "Grand Trunk"

                c2 =
                    "Erie"

                c3 =
                    "B&O"

                market =
                    { playerOrder = [ p1, p2, p3, p4 ]
                    , companyOrder = [ c1, c2, c3 ]
                    , playerCash =
                        Dict.fromList
                            [ ( p1, 100 )
                            , ( p2, 200 )
                            , ( p3, 500 )
                            , ( p4, 1 )
                            ]
                    , playerShares =
                        Dict.fromList
                            [ ( p1
                              , Dict.fromList
                                    [ ( c1, 2 )
                                    , ( c2, 1 )
                                    ]
                              )
                            , ( p2, Dict.fromList [ ( c2, 4 ) ] )
                            , ( p4, Dict.fromList [ ( c3, 2 ) ] )
                            ]
                    , presidents =
                        Dict.fromList
                            [ ( c1, p1 )
                            , ( c2, p2 )
                            , ( c3, p4 )
                            ]
                    , marketShares =
                        Dict.fromList
                            [ ( c2, 3 )
                            , ( c3, 3 )
                            ]
                    , companyCash =
                        Dict.fromList
                            [ ( c1, 300 )
                            , ( c2, 150 )
                            , ( c3, 5 )
                            ]
                    , shareValues =
                        Dict.fromList
                            [ ( c1, 15 )
                            , ( c2, 20 )
                            , ( c3, 120 )
                            ]
                    , bank = 1000
                    , totalShares = 10
                    , certificateLimit = 3
                    }
            in
            [ describe "playerCertificateCount"
                [ test "test 1" <|
                    \_ ->
                        playerCertificateCount market p1 |> Expect.equal 2
                , test "test 2" <|
                    \_ ->
                        playerCertificateCount market p2 |> Expect.equal 3
                , test "test 3" <|
                    \_ ->
                        playerCertificateCount market p3 |> Expect.equal 0
                ]
            , describe "playerStockValue"
                [ test "test 1" <|
                    \_ ->
                        playerStockValue market p1 |> Expect.equal 50
                , test "test 2" <|
                    \_ ->
                        playerStockValue market p2 |> Expect.equal 80
                , test "test 3" <|
                    \_ ->
                        playerStockValue market p3 |> Expect.equal 0
                ]
            , describe "playerNetWorth"
                [ test "test 1" <|
                    \_ ->
                        playerNetWorth market p1 |> Expect.equal 150
                , test "test 2" <|
                    \_ ->
                        playerNetWorth market p2 |> Expect.equal 280
                ]
            , describe "companyShares"
                [ test "test 1" <|
                    \_ ->
                        companyShares market c1 |> Expect.equal 8
                , test "test 2" <|
                    \_ ->
                        companyShares market c2 |> Expect.equal 2
                ]
            , describe "buyShareFromMarket"
                [ test "transaction test 1" <|
                    \_ ->
                        buyShareFromMarket p1 c2 market
                            |> Expect.all
                                [ Result.map (\m -> playerStockValue m p1)
                                    >> Expect.equal (Ok 70)
                                , Result.map (\m -> companyShares m c2)
                                    >> Expect.equal (Ok 2)
                                , Result.map (.marketShares >> Dict.get c2)
                                    >> Expect.equal (Ok (Just 2))
                                , Result.map (.playerShares >> Dict.get2 p1 c2)
                                    >> Expect.equal (Ok (Just 2))
                                , Result.map (.playerCash >> Dict.get p1)
                                    >> Expect.equal (Ok (Just 80))
                                , Result.map .bank
                                    >> Expect.equal (Ok 1020)
                                ]
                , test "transaction test 2" <|
                    \_ ->
                        buyShareFromMarket p3 c2 market
                            |> Expect.all
                                [ Result.map (.marketShares >> Dict.get c2)
                                    >> Expect.equal (Ok (Just 2))
                                , Result.map (.playerShares >> Dict.get2 p3 c2)
                                    >> Expect.equal (Ok (Just 1))
                                ]
                , test "certificate limit" <|
                    \_ ->
                        buyShareFromMarket p2 c2 market |> Expect.err
                , test "cash available" <|
                    \_ ->
                        buyShareFromMarket p4 c2 market |> Expect.err
                , test "share available" <|
                    \_ ->
                        buyShareFromMarket p3 c1 market |> Expect.err
                , test "presidency transfer" <|
                    \_ ->
                        market
                            |> buyShareFromMarket p3 c3
                            |> Result.andThen (buyShareFromMarket p3 c3)
                            |> Result.andThen (buyShareFromMarket p3 c3)
                            |> Result.map (.presidents >> Dict.get c3)
                            |> Expect.equal (Ok (Just p3))
                ]
            ]
        ]
