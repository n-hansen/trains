module StockMarket.RenderTest exposing (suite)

import StockMarket.Render exposing (..)
import Expect
import Test exposing (..)
import Tree


suite : Test
suite =
    describe "StockMarket.Render module"
        [ describe "CSS Grid"
              [ describe "buildLineSpec"
                    [ test "single section" <|
                        \_ ->
                            Tree.singleton "section"
                                |> buildLineSpec
                                |> Expect.equal [ "[begin-section]"
                                                , "[end-section]"
                                                ]
                    , test "section with children" <|
                        \_ ->
                            Tree.tree "top"
                                [ Tree.singleton "middle1"
                                , Tree.tree "middle2"
                                    [ Tree.singleton "bottom1"
                                    , Tree.singleton "bottom!"
                                    ]
                                ]
                                |> buildLineSpec
                                |> Expect.equal [ "[begin-middle1 begin-top]"
                                                , "[begin-bottom1 begin-middle2 end-middle1]"
                                                , "[begin-bottom_ end-bottom1]"
                                                , "[end-bottom_ end-middle2 end-top]"
                                                ]
                    ]
              ]
        ]
