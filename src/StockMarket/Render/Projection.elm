module StockMarket.Render.Projection exposing (..)


import LineChart
import LineChart.Dots as Dots
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import StockMarket as SM exposing (Action(..), Market, Projection)
import StockMarket.Render.Types exposing (..)
import Svg.Styled as Svg
import Tuple


renderProjection : RenderContext msg -> Html msg
renderProjection { projection, market, getColor} =
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
