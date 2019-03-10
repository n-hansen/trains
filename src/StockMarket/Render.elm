module StockMarket.Render exposing (..)

import Basics.Extra as Basics
import Css exposing (Style)
import Dict exposing (Dict)
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import List
import List.Extra as List
import Murmur3
import StockMarket exposing (Action, Market, Projection)
import StockMarket.Render.Types exposing (..)
import StockMarket.Render.Spreadsheet exposing (..)
import String


createRenderContext : (Action -> msg)
                    -> (Projection -> msg)
                    -> Market
                    -> Projection
                    -> RenderContext msg
createRenderContext actionMessage updateProjection market projection =
    let
        colorMap = assignColors market
    in
        { market = market
        , projection = projection
        , actionMessage = actionMessage
        , updateProjection = updateProjection
        , getColor = (\name ->
                          case Dict.get name colorMap of
                              Just c -> c
                              Nothing -> { primary = Css.hex "#ffffff"
                                         , secondary = Css.hex "#8c8c8c"
                                         }
                     )
        }


renderMarket : RenderContext msg -> Html msg
renderMarket ctx =
    Html.div
        [ Attr.css [ Css.displayFlex
                   , Css.flexDirection Css.column
                   ]
        ]
        [ renderSpreadsheet ctx
        ]


assignColors : Market -> Dict String { primary : Css.Color
                                     , secondary : Css.Color
                                     }
assignColors {playerOrder,companyOrder} =
    let
        sortedPlayers = List.sort playerOrder
        sortedCompanies = List.sort companyOrder
        seed = String.join "#" sortedPlayers
                   |> Murmur3.hashString 42
                   |> modBy 9989899
                   |> toFloat
                   |> (*) (360.0 / 9989899.0)
        next x = x + (360 * 0.618033988749895)
                     |> Basics.fractionalModBy 360
    in
        sortedPlayers ++ sortedCompanies
            |> List.mapAccuml
               (\hue name ->
                    ( next hue
                    , ( name
                      , { primary = Css.hsl hue 0.9 0.65
                        , secondary = Css.hsl hue 0.85 0.82
                        }
                      )
                    )
               ) seed
            |> Tuple.second
            |> Dict.fromList

