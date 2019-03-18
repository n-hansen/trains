module StockMarket.ParserTest exposing (suite)


import AssocList as Dict
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
                          ["certificates: 20"]
                          { blankState
                              | certificateLimit = Just 20
                          }
              , test "commentParser 1" <|
                  \_ ->
                      checkParse
                          ["bank: $600"
                          ,"#foo bar baz quux"
                          ,"certificates: 11"
                          ]
                          { blankState
                              | bank = Just 600
                              , certificateLimit = Just 11
                          }
              , test "commentParser 2" <|
                  \_ ->
                      checkParse
                          ["bank: $600"
                          ," "
                          ," #foo bar baz quux"
                          ,"certificates: 11"
                          ]
                          { blankState
                              | bank = Just 600
                              , certificateLimit = Just 11
                          }

              ]
        , describe "materialization tests"
            [ test "mat test 1" <|
                \_ ->
                    checkMaterialization
                        ["certs:11"
                        ,"c: NYC $100, bank:1, alice: 2, bob*:3"
                        ,"c: GT $200, alice*:4"
                        ,"b: $400"
                        ,"p: alice $11"
                        ,"p: bob* $12"
                        ]
                        ( Ok <| Market
                              [P "alice", P "bob"]
                              (Just <| P "bob")
                              [C "NYC", C "GT"]
                              ( Dict.fromList
                                    [ (P "bob", 12)
                                    , (P "alice", 11)
                                    ]
                              )
                              ( Dict.fromList
                                    [ ((P "alice", C "NYC"), 2)
                                    , ((P "bob", C "NYC"), 3)
                                    , ((P "alice", C "GT"), 4)
                                    ]
                              )
                              ( Dict.fromList
                                    [ (C "GT", P "alice")
                                    , (C "NYC", P "bob")
                                    ]
                              )
                              ( Dict.fromList
                                    [(C "NYC", 1)]
                              )
                              (Dict.fromList [(C "GT", 0),(C "NYC", 0)])
                              (linearShareValueTrack [0,10,20,30,40,50,60,70,80,90,100,112,124,137,150,165,180,195,212,230,250,270,295,320,345,375,405,440,475,510,500]
                                   |> insertCompanyShareValue (C "NYC") 100
                                   |> insertCompanyShareValue (C "GT") 200
                              )
                              400
                              10
                              11
                        )
            , test "mat test 2" <|
                \_ ->
                    checkMaterialization
                        ["c: NYC $100, bank:1, alice: 2, bob*:3"
                        ,"c: GT $200, alice*:4"
                        ,"b: $400"
                        ,"p: alice $11"
                        ,"p: bob* $12"
                        ]
                        ( Err ["Please set the certificate limit."])
            , test "mat test 3" <|
                \_ ->
                    checkMaterialization
                        ["c: NYC $100, bank:1, alice: 2, bob*:3"
                        ,"c: GT $200, alice*:4"
                        ,"p: alice $11"
                        ,"p: bob* $12"
                        ]
                        ( Err ["Please set the certificate limit."
                              ,"Please set the amount of money in the bank."])
            , test "mat test 4" <|
                \_ ->
                    checkMaterialization
                        ["certs:11"
                        ,"c: NYC $100, bank:1, alice: 2, bob*:3"
                        ,"c: GT $200, alic*:4"
                        ,"b: $400"
                        ,"p: alice $11"
                        ,"p: bob* $12"
                        ]
                        ( Err ["Player alic owns stock in GT but is missing a declaration."]
                        )
            , test "mat test 5" <|
                \_ ->
                    checkMaterialization
                        ["certs:11"
                        ,"c: NYC $100, bank:1, alice: 2, bob*:3"
                        ,"c: GT $200, alic*:4"
                        ,"b: $400"
                        ,"p: alice $11"
                        ,"p: bob* $12, C&O: 1"
                        ]
                        ( Err ["Company C&O is owned by bob but is missing a declaration."
                              ,"Player alic owns stock in GT but is missing a declaration."
                              ]
                        )
            -- , test "mat test 6" <|
            --     \_ ->
            --         checkMaterialization
            --             ["certs:11"
            --             ,"c: NYC $100, bank:1, alice: 3, bob*:2"
            --             ,"c: GT $200, alice*:4"
            --             ,"b: $400"
            --             ,"p: alice $11"
            --             ,"p: bob* $12"
            --             ]
            --             ( Err ["Player alic is the share leader of the NYC but is not president."]
            --             )
            ]
        -- TODO write market equality that handles alists better
        -- , describe "serialization tests"
        --     [ test "serialize test 1" <|
        --         \_ ->
        --           checkSerializeThenDeserialize
        --           <| Market
        --                 [P "alice", P "bob"]
        --                 (Just <| P "bob")
        --                 [C "NYC", C "GT"]
        --                 ( Dict.fromList
        --                       [ (P "bob", 12)
        --                       , (P "alice", 11)
        --                       ]
        --                 )
        --                 ( Dict.fromList
        --                       [ ((P "alice", C "NYC"), 2)
        --                       , ((P "bob", C "NYC"), 3)
        --                       , ((P "alice", C "GT"), 4)
        --                       ]
        --                 )
        --                 ( Dict.fromList
        --                       [ (C "GT", P "alice")
        --                       , (C "NYC", P "bob")
        --                       ]
        --                 )
        --                 ( Dict.fromList
        --                       [(C "NYC", 1)]
        --                 )
        --                 Dict.empty
        --                 (linearShareValueTrack [0,10,20,30,40,50,60,70,80,90,100,112,124,137,150,165,180,195,212,230,250,270,295,320,345,375,405,440,475,510,500]
        --                 |> insertCompanyShareValue (C "NYC") 100
        --                 |> insertCompanyShareValue (C "GT") 200
        --                 )
        --                 400
        --                 10
        --                 11
        --     ]
        ]


checkParse : List String -> ParseState -> Expectation
checkParse lines expectedState =
    lines
        |> String.join "\n"
        |> Parser.run configurationParser
        |> Result.withDefault []
        |> List.foldl (<|) blankState
        |> Expect.equal expectedState


checkMaterialization : List String -> Result (List String) Market -> Expectation
checkMaterialization lines expected =
    lines
        |> String.join "\n"
        |> parseAndMaterializeMarket
        |> Expect.equal expected


checkSerializeThenDeserialize : Market -> Expectation
checkSerializeThenDeserialize market =
    market
        |> serialize
        |> parseAndMaterializeMarket
        |> Expect.equal (Ok market)
