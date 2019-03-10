module Main exposing (main)

import Browser
import Html.Styled as Html exposing (Html)
import Result
import StockMarket as SM
import StockMarket.Render as SM


main =
    Browser.sandbox { init = init
                    , view = view >> Html.toUnstyled
                    , update = update
                    }

init : SM.Market
init =
    SM.emptyMarket 1000 5 8
        |> SM.addPlayer "Alice" 200
        |> SM.addPlayer "Bob" 300
        |> SM.addCompany "Grand Trunk" 25
        |> SM.addCompany "NYC" 40
        |> SM.buyShareFromCompany "Alice" "Grand Trunk"
        |> Result.andThen (SM.buyShareFromCompany "Alice" "Grand Trunk")
        |> Result.andThen (SM.buyShareFromCompany "Alice" "Grand Trunk")
        |> Result.andThen (SM.buyShareFromCompany "Bob" "Grand Trunk")
        |> Result.andThen (SM.buyShareFromCompany "Bob" "NYC")
        |> Result.andThen (SM.buyShareFromCompany "Bob" "NYC")
        |> Result.withDefault (SM.emptyMarket 1000 5 8)


view = SM.renderSpreadsheet


update msg model = model
