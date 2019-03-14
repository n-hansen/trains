module StockMarket.Render.Types exposing (..)

import AssocList as Dict exposing (Dict)
import Color
import StockMarket exposing (..)


type alias ProjectionInput = Dict CompanyName String


type alias RenderContext msg =
    { market : Market
    , projectionInput : ProjectionInput
    , actionMessage : Action -> msg
    , updateProjectionInput : ProjectionInput -> msg
    , getColor : String -> { primary : Color.Color, secondary : Color.Color }
    }
