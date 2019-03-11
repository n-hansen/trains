module Main exposing (main)

import Browser
import Debug
import Dict
import Html.Styled as Html exposing (Html)
import Result
import StockMarket as SM exposing (Action(..))
import StockMarket.Render as SM
import StockMarket.Render.Types as SM


main =
    Browser.sandbox { init = init
                    , view = view >> Html.toUnstyled
                    , update = update
                    }


type alias Model =
    { market : SM.Market
    , projectionInput : SM.ProjectionInput
    }


init : Model
init =
    let
        emptyMarket = SM.emptyMarket (SM.linearShareValueTrack [0,10,25,40,50,60,75]) 1000 5 8
        market =
            emptyMarket
                |> SM.addPlayer "Alice" 200
                |> SM.addPlayer "Bob" 300
                |> SM.addCompany "Grand Trunk" 25
                |> SM.addCompany "NYC" 40
                |> SM.tryAction (SM.Batch
                                     [ SM.BuyShareFromCompany "Alice" "Grand Trunk"
                                     , SM.BuyShareFromCompany "Alice" "Grand Trunk"
                                     , SM.BuyShareFromCompany "Bob" "NYC"
                                     , SM.BuyShareFromCompany "Bob" "NYC"
                                     ]
                                )
                |> Result.withDefault emptyMarket
        projectionInput = Dict.empty
    in
        { market = market
        , projectionInput = projectionInput
        }


type Message = MarketAction SM.Action
             | UpdateProjection SM.ProjectionInput


view {market, projectionInput} =
    SM.createRenderContext
        MarketAction
        UpdateProjection
        market
        projectionInput
        |> SM.renderMarket


update message model =
    case message of
        MarketAction axn ->
            model.market
                |> SM.tryAction axn
                |> Result.map (\mkt -> {model | market = mkt})
                |> Result.mapError (Debug.log "Update Error: ")
                |> Result.withDefault model

        UpdateProjection proj ->
            { model
                | projectionInput = proj
            }
