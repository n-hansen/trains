module Main exposing (main)

import Browser
import Html.Styled as Html exposing (Html)
import StockMarket as SM
import StockMarket.Render as SM


main =
    Browser.sandbox { init = init
                    , view = view >> Html.toUnstyled
                    , update = update
                    }

init : SM.Market
init = SM.emptyMarket 1000 5 8


view = SM.renderSpreadsheet


update msg model = model
