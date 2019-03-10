module StockMarket.Render.Types exposing (..)

import Css
import StockMarket exposing (..)


type alias RenderContext msg =
    { market : Market
    , projection : Projection
    , actionMessage : Action -> msg
    , updateProjection : Projection -> msg
    , getColor : String -> { primary : Css.Color, secondary : Css.Color }
    }
