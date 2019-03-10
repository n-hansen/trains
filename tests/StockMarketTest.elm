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
        [ describe "Market construction"
              [ test "add player" <|
                  \_ ->
                      emptyMarket 1000 5 8
                          |> addPlayer "Alice" 100
                          |> addPlayer "Bob" 150
                          |> Expect.all [ .playerOrder >> Expect.equal ["Alice","Bob"]
                                        , .playerCash
                                              >> Dict.get "Alice"
                                              >> Expect.equal (Just 100)
                                        , .playerCash
                                              >> Dict.get "Bob"
                                              >> Expect.equal (Just 150)
                                        , .bank >> Expect.equal 750
                                        ]
              , test "add company" <|
                  \_ ->
                      emptyMarket 1000 5 8
                          |> addCompany "Grand Trunk" 50
                          |> addCompany "NYC" 60
                          |> Expect.all [ .companyOrder >> Expect.equal ["Grand Trunk", "NYC"]
                                        , .companyCash
                                              >> Dict.get "Grand Trunk"
                                              >> Expect.equal (Just 0)
                                        , .companyCash
                                              >> Dict.get "NYC"
                                              >> Expect.equal (Just 0)
                                        , .shareValues
                                              >> Dict.get "Grand Trunk"
                                              >> Expect.equal (Just 50)
                                        , .shareValues
                                              >> Dict.get "NYC"
                                              >> Expect.equal (Just 60)
                                        ]
              ]
        , describe "Simple market example" <|
            let
                p1 = "Alice"
                p2 = "Bob"
                p3 = "Candice"
                p4 = "Doug"
                c1 = "Grand Trunk"
                c2 = "Erie"
                c3 = "B&O"
                market =
                    { playerOrder = [ p1, p2, p3, p4 ]
                    , companyOrder = [c1, c2, c3]
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
                    , bankShares =
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
            , describe "buyShareFromBank"
                [ test "transaction test 1" <|
                    \_ ->
                        market
                            |> tryAction (BuyShareFromBank p1 c2)
                            |> Expect.all
                                [ Result.map (\m -> playerStockValue m p1)
                                    >> Expect.equal (Ok 70)
                                , Result.map (\m -> companyShares m c2)
                                    >> Expect.equal (Ok 2)
                                , Result.map (.bankShares >> Dict.get c2)
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
                        market
                            |> tryAction (BuyShareFromBank p3 c2)
                            |> Expect.all
                                [ Result.map (.bankShares >> Dict.get c2)
                                    >> Expect.equal (Ok (Just 2))
                                , Result.map (.playerShares >> Dict.get2 p3 c2)
                                    >> Expect.equal (Ok (Just 1))
                                ]
                , test "certificate limit" <|
                    \_ ->
                        market |> tryAction (BuyShareFromBank p2 c2) |> Expect.err
                , test "cash available" <|
                    \_ ->
                        market |> tryAction (BuyShareFromBank p4 c2) |> Expect.err
                , test "share available" <|
                    \_ ->
                        market |> tryAction (BuyShareFromBank p3 c1) |> Expect.err
                , test "presidency transfer 1" <|
                    \_ ->
                        market
                            |> tryAction (Batch [ BuyShareFromBank p3 c3
                                                , BuyShareFromBank p3 c3
                                                , BuyShareFromBank p3 c3
                                                ]
                                         )
                            |> Result.map (.presidents >> Dict.get c3)
                            |> Expect.equal (Ok (Just p3))
                , test "presidency transfer 2" <|
                    \_ ->
                        market
                            |> tryAction (Batch [ BuyShareFromBank p3 c3
                                                , BuyShareFromBank p3 c3
                                                ]
                                         )
                            |> Result.map (.presidents >> Dict.get c3)
                            |> Expect.equal (Ok (Just p4))

                ]
            , describe "buyShareFromCompany"
                [ test "transaction test" <|
                    \_ ->
                        market
                            |> tryAction (BuyShareFromCompany p1 c2)
                            |> Expect.all
                                [ Result.map (.playerShares >> Dict.get2 p1 c2)
                                    >> Expect.equal (Ok (Just 2))
                                , Result.map (\m -> companyShares m c2)
                                    >> Expect.equal (Ok 1)
                                , Result.map (.playerCash >> Dict.get p1)
                                    >> Expect.equal (Ok (Just 80))
                                , Result.map (.companyCash >> Dict.get c2)
                                    >> Expect.equal (Ok (Just 170))
                                ]
                , test "certificate limit" <|
                    \_ ->
                        market |> tryAction (BuyShareFromCompany p2 c2) |> Expect.err
                , test "cash available" <|
                    \_ ->
                        market |> tryAction (BuyShareFromCompany p4 c2) |> Expect.err
                , test "share available" <|
                    \_ ->
                        market
                            |> tryAction (Batch [ BuyShareFromCompany p3 c2
                                                , BuyShareFromCompany p3 c2
                                                , BuyShareFromCompany p3 c2
                                                ]
                                         )
                            |> Expect.err
                , test "presidency transfer 1" <|
                    \_ ->
                        market
                            |> tryAction (Batch [ BuyShareFromCompany p3 c3
                                                , BuyShareFromCompany p3 c3
                                                , BuyShareFromCompany p3 c3
                                                ]
                                         )
                            |> Result.map (.presidents >> Dict.get c3)
                            |> Expect.equal (Ok (Just p3))
                , test "presidency transfer 2" <|
                    \_ ->
                        market
                            |> tryAction (Batch [ BuyShareFromCompany p3 c3
                                                , BuyShareFromCompany p3 c3
                                                ]
                                         )
                            |> Result.map (.presidents >> Dict.get c3)
                            |> Expect.equal (Ok (Just p4))

                , test "company launch" <|
                    \_ ->
                        market
                            |> addCompany "NYC" 1
                            |> tryAction (Batch [ BuyShareFromCompany p3 "NYC"
                                                , BuyShareFromCompany p3 "NYC"
                                                ]
                                         )
                            |> Result.map (.presidents >> Dict.get "NYC")
                            |> Expect.equal (Ok (Just p3))
                ]
            ]
        ]
