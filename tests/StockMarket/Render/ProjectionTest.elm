module StockMarket.Render.ProjectionTest exposing (suite)

import Dict
import StockMarket exposing (..)
import StockMarket.Render.Types exposing (..)
import StockMarket.Render.Projection exposing (..)
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "StockMarket.Render.Projection module"
        [ describe "input parser"
              [ describe "buildCompanyProjection"
                    [ test "test 1" <|
                        \_ ->
                            ("NYC", ">")
                                |> buildCompanyProjection
                                |> Expect.equal (Just [[MoveShareValueRight "NYC"]])
                    , test "test 2" <|
                        \_ ->
                            ("NYC", "<")
                                |> buildCompanyProjection
                                |> Expect.equal (Just [[MoveShareValueLeft "NYC"]])
                    , test "test 3" <|
                        \_ ->
                            ("NYC", "><")
                                |> buildCompanyProjection
                                |> Expect.equal (Just [[MoveShareValueRight "NYC"
                                                       ,MoveShareValueLeft "NYC"
                                                       ]])
                    , test "test 4" <|
                        \_ ->
                            ("NYC", "><,<>")
                                |> buildCompanyProjection
                                |> Expect.equal (Just [ [ MoveShareValueRight "NYC"
                                                        , MoveShareValueLeft "NYC"
                                                        ]
                                                      , [ MoveShareValueLeft "NYC"
                                                        , MoveShareValueRight "NYC"
                                                        ]
                                                      ]
                                                )
                    , test "test 5" <|
                        \_ ->
                            ("NYC","120")
                                |> buildCompanyProjection
                                |> Expect.equal (Just [[PayDividend "NYC" 120]])
                    ]
              , describe "buildProjection"
                  [ test "test 1" <|
                      \_ ->
                          Dict.fromList [ ("NYC", ">") ]
                              |> buildProjection
                              |> Expect.equal (Just [[MoveShareValueRight "NYC"]])
                  , test "test 2" <|
                      \_ ->
                          Dict.fromList [ ("NYC", ">,<")
                                        , ("IC" , ",>")
                                        , ("B&O", "<<")
                                        ]
                              |> buildProjection
                              |> Expect.equal ( Just [ [ MoveShareValueLeft "B&O"
                                                       , MoveShareValueLeft "B&O"
                                                       , MoveShareValueRight "NYC"
                                                       ]
                                                     , [ MoveShareValueRight "IC"
                                                       , MoveShareValueLeft "NYC"
                                                       ]
                                                    ]
                                              )
                  ]
              ]
        ]
