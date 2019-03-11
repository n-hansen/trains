module StockMarket.Render.Types exposing (..)

import Color
import StockMarket exposing (..)


type alias RenderContext msg =
    { market : Market
    , projection : Projection
    , actionMessage : Action -> msg
    , updateProjection : Projection -> msg
    , getColor : String -> { primary : Color.Color, secondary : Color.Color }
    }
