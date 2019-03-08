module StockMarketTest exposing (..)

import Dict
import StockMarket exposing (..)
import Test exposing (..)
import Expect

suite : Test
suite =
    describe "StockMarket module"
        [ describe "Simple market example" <|
              let p1 = "Alice"
                  p2 = "Bob"
                  p3 = "Candice"
                  c1 = "Grand Trunk"
                  c2 = "Erie"
                  market = { playerOrder = [p1,p2,p3]
                           , companyOrder = [c1,c2]
                           , playerCash = Dict.fromList [ (p1,100)
                                                        , (p2,200)
                                                        , (p3,500)]
                           , playerShares = Dict.fromList [ (p1, Dict.fromList [ (c1,2)
                                                                               , (c2,1)])
                                                          , (p2, Dict.fromList [(c2,4)])
                                                          ]
                           , presidents = Dict.fromList [ (c1, p1)
                                                        , (c2, p2)]
                           , marketShares = Dict.fromList [(c1, 3)]
                           , companyCash = Dict.fromList [ (c1, 300)
                                                         , (c2, 150)
                                                         ]
                           , shareValues = Dict.fromList [ (c1, 15)
                                                         , (c2, 20)
                                                         ]
                           , bank = 1000
                           , totalShares = 10
                           }
              in [ describe "playerCertificateCount"
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
                 ]
        ]
