module StockMarket.ParserTest exposing (suite)


import Expect exposing (Expectation)
import List
import Parser
import Result
import StockMarket exposing (..)
import StockMarket.Parser exposing (..)
import String
import Test exposing (..)


suite : Test
suite =
    describe "Parser module"
        [ describe "parse tests"
              [ test "bankValueParser" <|
                  \_ ->
                      checkParse
                          ["bank: $600"]
                          { blankState | bank = Just 600}
              , test "playerParser 1" <|
                  \_ ->
                      checkParse
                          ["player: alice $600"]
                          { blankState
                              | playerOrder = [P "alice"]
                              , playerCash = [(P "alice", 600)]
                          }
              , test "playerParser 2" <|
                  \_ ->
                      checkParse
                          ["player: alice* $500"]
                          { blankState
                                | playerOrder = [P "alice"]
                                , playerCash = [(P "alice", 500)]
                                , activePlayer = Just <| P "alice"
                          }
              , test "playerParser 3" <|
                  \_ ->
                      checkParse
                          ["player: alice $200"
                          ,"player: bob $300"
                          ]
                          { blankState
                                | playerOrder = [P "bob",P "alice"]
                                , playerCash = [(P "bob",300)
                                               ,(P "alice",200)]
                          }
              , test "playerParser 4" <|
                  \_ ->
                      checkParse
                          ["player: alice $200, B&O=2"]
                          { blankState
                                | playerOrder = [P "alice"]
                                , playerCash = [(P "alice",200)]
                                , playerShares = [((P "alice",C "B&O"),2)]
                          }
              , test "playerParser 5" <|
                  \_ ->
                      checkParse
                          ["player: alice $200, B&O=2, C&O=3"]
                          { blankState
                              | playerOrder = [P "alice"]
                              , playerCash = [(P "alice",200)]
                              , playerShares = [((P "alice",C "B&O"),2)
                                               ,((P "alice",C "C&O"),3)
                                               ]
                          }
              , test "playerParser 6" <|
                  \_ ->
                      checkParse
                          ["player: alice $200, B&O*=2"]
                          { blankState
                                | playerOrder = [P "alice"]
                                , playerCash = [(P "alice",200)]
                                , playerShares = [((P "alice",C "B&O"),2)]
                                , presidents = [(C "B&O", P "alice")]
                          }

              , test "companyParser 1" <|
                  \_ ->
                      checkParse
                          ["company: C&O $100"]
                          { blankState
                              | companyOrder = [C "C&O"]
                              , shareValues = [(C "C&O",100)]
                          }
              , test "companyParser 2" <|
                  \_ ->
                      checkParse
                          ["company: C&O $100, alice=2"]
                          { blankState
                              | companyOrder = [C "C&O"]
                              , shareValues = [(C "C&O",100)]
                              , playerShares = [((P "alice",C "C&O"),2)]
                          }
              , test "companyParser 3" <|
                  \_ ->
                      checkParse
                          ["company: C&O $100, alice=2, bob=3"]
                          { blankState
                              | companyOrder = [C "C&O"]
                              , shareValues = [(C "C&O",100)]
                              , playerShares = [((P "alice", C "C&O"),2)
                                               ,((P "bob", C "C&O"),3)
                                               ]
                          }
              , test "companyParser 4" <|
                  \_ ->
                      checkParse
                          ["company: C&O $100, treasury=1, alice=2, bob=3"]
                          { blankState
                              | companyOrder = [C "C&O"]
                              , shareValues = [(C "C&O",100)]
                              , playerShares = [((P "alice", C "C&O"),2)
                                               ,((P "bob", C "C&O"),3)
                                               ]
                              , bankShares = [(C "C&O",1)]
                          }
              , test "companyParser 5" <|
                  \_ ->
                      checkParse
                          ["company: C&O $100, alice* = 2"]
                          { blankState
                              | companyOrder = [C "C&O"]
                              , shareValues = [(C "C&O",100)]
                              , playerShares = [((P "alice",C "C&O"),2)]
                              , presidents = [(C "C&O", P "alice")]
                          }
              , test "certificateCountParser" <|
                  \_ ->
                      checkParse
                          ["certificates: 10"]
                          { blankState
                              | certificateLimit = Just 10
                          }
              ]
        ]


checkParse : List String -> ParseState -> Expectation
checkParse lines expectedState =
    lines
        |> String.join "\n"
        |> Parser.run configurationParser
        |> Result.withDefault []
        |> List.foldl (<|) blankState
        |> Expect.equal expectedState
