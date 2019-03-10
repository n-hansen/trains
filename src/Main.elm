module Main exposing (main)

import Browser
import Debug
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
        |> SM.tryAction (SM.Batch
                             [ SM.BuyShareFromCompany "Alice" "Grand Trunk"
                             , SM.BuyShareFromCompany "Alice" "Grand Trunk"
                             , SM.BuyShareFromCompany "Bob" "Grand Trunk"
                             ]
                        )
        |> Result.withDefault (SM.emptyMarket 1000 5 8)


type Message = SmAction SM.Action


view = SM.renderSpreadsheet SmAction


update (SmAction axn) model =
    model
        |> SM.tryAction axn
        |> Result.mapError Debug.log
        |> Result.withDefault model
