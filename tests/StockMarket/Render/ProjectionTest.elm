module StockMarket.Render.ProjectionTest exposing (suite)

import AssocList as Dict
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
                            (C "NYC", ">")
                                |> buildCompanyProjection
                                |> Expect.equal (Just [[MoveShareValueRight <| C "NYC"]])
                    , test "test 2" <|
                        \_ ->
                            (C "NYC", "<")
                                |> buildCompanyProjection
                                |> Expect.equal (Just [[MoveShareValueLeft <| C "NYC"]])
                    , test "test 3" <|
                        \_ ->
                            (C "NYC", "><")
                                |> buildCompanyProjection
                                |> Expect.equal (Just [[MoveShareValueRight <| C "NYC"
                                                       ,MoveShareValueLeft <| C "NYC"
                                                       ]])
                    , test "test 4" <|
                        \_ ->
                            (C "NYC", "><,<>")
                                |> buildCompanyProjection
                                |> Expect.equal (Just [ [ MoveShareValueRight <| C "NYC"
                                                        , MoveShareValueLeft <| C "NYC"
                                                        ]
                                                      , [ MoveShareValueLeft <| C "NYC"
                                                        , MoveShareValueRight <| C "NYC"
                                                        ]
                                                      ]
                                                )
                    , test "test 5" <|
                        \_ ->
                            (C "NYC","120")
                                |> buildCompanyProjection
                                |> Expect.equal (Just [[PayDividend (C "NYC") 120]])
                    , test "test 6" <|
                        \_ ->
                            (C "NYC", ">aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
                                |> buildCompanyProjection
                                |> Expect.equal Nothing
                    ]
              , describe "buildProjection"
                  [ test "test 1" <|
                      \_ ->
                          Dict.fromList [ (C "NYC", ">") ]
                              |> buildProjection
                              |> Expect.equal (Just [[MoveShareValueRight <| C "NYC"]])
                  , test "test 2" <|
                      \_ ->
                          Dict.fromList [ (C "NYC", ">,<")
                                        , (C "IC" , ",>")
                                        , (C "B&O", "<<")
                                        ]
                              |> buildProjection
                              |> Expect.equal ( Just [ [ MoveShareValueLeft <| C "B&O"
                                                       , MoveShareValueLeft <| C "B&O"
                                                       , MoveShareValueRight <| C "NYC"
                                                       ]
                                                     , [ MoveShareValueRight <| C "IC"
                                                       , MoveShareValueLeft <| C "NYC"
                                                       ]
                                                    ]
                                              )
                  ]
              ]
        ]
