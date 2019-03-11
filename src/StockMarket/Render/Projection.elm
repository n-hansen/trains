module StockMarket.Render.Projection exposing (..)

import Css
import Dict
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import LineChart
import LineChart.Dots as Dots
import List
import List.Extra as List
import Maybe
import Maybe.Extra as Maybe
import Parser exposing ((|=), (|.))
import Result
import StockMarket as SM exposing (Action(..), CompanyName, Market, PlayerName, Projection)
import StockMarket.Render.Types exposing (..)
import Svg.Styled as Svg
import Tuple


renderProjectionArea : RenderContext msg -> Html msg
renderProjectionArea ({projectionInput} as ctx) =
    [ Just <| projectionInputForm ctx
    , projectionInput
        |> buildProjection
        |> Maybe.map (renderProjectionChart ctx)
    ]
    |> Maybe.values
    |> Html.div
       [ Attr.css [ Css.displayFlex
                  , Css.flexDirection Css.row
                  , Css.alignItems Css.flexStart
                  ]
       ]



-- View components


renderProjectionChart : RenderContext msg -> Projection -> Html msg
renderProjectionChart { market, getColor } projection =
    let
        markets = SM.runProjection projection market
        dataseries player =
            List.indexedMap
                (\ix m -> (toFloat ix, SM.playerNetWorth m player |> toFloat))
                markets
        chart =
            market.playerOrder
                |> List.map (\player ->
                                 LineChart.line
                                 (getColor player |> .primary)
                                 Dots.circle
                                 player
                                 (dataseries player)
                            )
                |> LineChart.view Tuple.first Tuple.second

    in
        Svg.fromUnstyled chart


projectionInputForm : RenderContext msg -> Html msg
projectionInputForm { market, projectionInput, updateProjectionInput } =
    market.companyOrder
        |> List.map
           ( \company ->
                 Html.div
                 []
                 [ Html.span
                       []
                       [ Html.text <| company ++ ": "
                       ]
                 , Html.input
                     [ Dict.get company projectionInput
                           |> Maybe.withDefault ""
                           |> Attr.value
                     , Events.onInput
                         <| \i -> updateProjectionInput (Dict.insert company i projectionInput)
                     ]
                     []
                 ]
           )
        |> (++) [ Html.div
                      [ Attr.css [ Css.alignSelf Css.center
                                 , Css.fontSize Css.large
                                 ]
                      ]
                      [ Html.text "Operating round projections:"]
                ]
        |> Html.div
           [Attr.css [ Css.displayFlex
                     , Css.flexDirection Css.column
                     , Css.alignItems Css.flexEnd
                     ]
           ]



-- Input parsing


buildCompanyProjection : (CompanyName, String) -> Maybe Projection
buildCompanyProjection (company, input) =
    let
        parser =
            Parser.loop ([],[]) loopHelper

        loopHelper (currRound,pastRounds) =
            Parser.oneOf
                [ Parser.succeed (\axn -> Parser.Loop (axn::currRound,pastRounds))
                     |= actionParser
                , Parser.succeed (\_ -> Parser.Loop ([], List.reverse currRound::pastRounds))
                     |= roundSeparator
                , Parser.succeed ()
                     |> Parser.map (\_ ->
                                        (List.reverse currRound :: pastRounds)
                                            |> List.reverse
                                            |> Parser.Done
                                   )
                ]

        actionParser =
            Parser.oneOf
                [ Parser.succeed (MoveShareValueRight company)
                    |. Parser.symbol ">"
                , Parser.succeed (MoveShareValueLeft company)
                    |. Parser.symbol "<"
                , Parser.succeed (PayDividend company)
                    |= Parser.int
                ]

        roundSeparator = Parser.symbol ","

    in
        input
            |> Parser.run parser
            |> Result.toMaybe


buildProjection : ProjectionInput -> Maybe Projection
buildProjection projectionInput =
    let
        mergeProjections projs =
            let
                heads =
                    projs
                        |> List.map List.head
                        |> Maybe.values
                        |> List.concat
                tails =
                    projs
                        |> List.map List.tail
                        |> Maybe.values
                        |> List.filter (List.isEmpty >> not)
            in
                if List.isEmpty tails
                then [heads]
                else heads :: mergeProjections tails

    in
        projectionInput
            |> Dict.toList
            |> Maybe.traverse buildCompanyProjection
            |> Maybe.map mergeProjections
