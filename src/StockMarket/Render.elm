module StockMarket.Render exposing (..)

import Basics.Extra as Basics
import Color
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
import StockMarket.Render.Projection exposing (..)
import String
import Util.Color as Color


createRenderContext : (Action -> msg)
                    -> (ProjectionInput -> msg)
                    -> Market
                    -> ProjectionInput
                    -> RenderContext msg
createRenderContext actionMessage updateProjectionInput market projectionInput =
    let
        colorMap = assignColors market
    in
        { market = market
        , projectionInput = projectionInput
        , actionMessage = actionMessage
        , updateProjectionInput = updateProjectionInput
        , getColor = (\name ->
                          case Dict.get name colorMap of
                              Just c -> c
                              Nothing -> { primary = Color.grey
                                         , secondary = Color.white
                                         }
                     )
        }


renderMarket : RenderContext msg -> Html msg
renderMarket ctx =
    Html.div
        [ Attr.css [ Css.displayFlex
                   , Css.flexDirection Css.column
                   , Css.marginBottom <| Css.px 30
                   ]
        ]
        [ renderSpreadsheet ctx
        , renderProjectionArea ctx
        ]


assignColors : Market -> Dict String { primary : Color.Color
                                     , secondary : Color.Color
                                     }
assignColors {playerOrder,companyOrder} =
    let
        rotation = 0.618033988749895
        sortedPlayers = List.sort playerOrder
        sortedCompanies = List.sort companyOrder
        seedHue = String.join "#" sortedPlayers
                      |> Murmur3.hashString 42
                      |> modBy 9989899
                      |> toFloat
                      |> \x -> x / 9989899.0
        seed = { primary = Color.hsl seedHue 0.9 0.65
               , secondary = Color.hsl seedHue 0.85 0.82
               }
        next {primary,secondary} =
            { primary = Color.rotateBy rotation primary
            , secondary = Color.rotateBy rotation secondary
            }
    in
        sortedPlayers ++ sortedCompanies
            |> List.mapAccuml
               (\c name ->
                    ( next c
                    , (name , c)
                    )
               ) seed
            |> Tuple.second
            |> Dict.fromList

