module Main exposing (main)

import AssocList as Dict
import Browser
import Debug
import Html.Styled as Html exposing (Html)
import Result
import StockMarket as SM exposing (Action(..),PlayerName(..),CompanyName(..))
import StockMarket.Render as SM
import StockMarket.Render.Configure as Cfg
import StockMarket.Render.Types as SM


main =
    Browser.sandbox { init = init
                    , view = view >> Html.toUnstyled
                    , update = update
                    }


type Model =
    Spreadsheet { market : SM.Market
                , projectionInput : SM.ProjectionInput
                }
    | Configuration Cfg.State


init : Model
init = Configuration Cfg.blankState
    -- let
    --     emptyMarket = SM.emptyMarket (SM.linearShareValueTrack [0,10,25,40,50,60,75]) 1000 5 8
    --     market =
    --         emptyMarket
    --             |> SM.addPlayer (P "Alice") 200
    --             |> SM.addPlayer (P "Bob") 300
    --             |> SM.addCompany(C "Grand Trunk") 25
    --             |> SM.addCompany(C "NYC") 40
    --             |> SM.tryAction (SM.Batch
    --                                  [ SM.BuyShareFromCompany (P "Alice") <| C "Grand Trunk"
    --                                  , SM.BuyShareFromCompany (P "Alice") <| C "Grand Trunk"
    --                                  , SM.BuyShareFromCompany (P "Bob" ) <| C "NYC"
    --                                  , SM.BuyShareFromCompany (P "Bob" ) <| C "NYC"
    --                                  ]
    --                             )
    --             |> Result.withDefault emptyMarket
    --     projectionInput = Dict.empty
    -- in
    --     { market = market
    --     , projectionInput = projectionInput
    --     }


type Message = SubmitConfiguration SM.Market
             | UpdateConfiguration Cfg.State
             | MarketAction SM.Action
             | UpdateProjection SM.ProjectionInput


view model =
    case model of
        Configuration cfg ->
            Cfg.configurationInput UpdateConfiguration SubmitConfiguration cfg

        Spreadsheet {market, projectionInput} ->
            SM.createRenderContext
                MarketAction
                UpdateProjection
                market
                projectionInput
                    |> SM.renderMarket


update message model =
    case model of
        Configuration cfg ->
            case message of
                UpdateConfiguration newCfg ->
                    Configuration newCfg

                SubmitConfiguration mkt ->
                    Spreadsheet { market = mkt
                                , projectionInput = Dict.empty
                                }

                _ -> model

        Spreadsheet ({market, projectionInput} as ss) ->
            Spreadsheet <|
                case message of
                    MarketAction axn ->
                        market
                            |> SM.tryAction axn
                            |> Result.map (\m -> { ss | market = m })
                            |> Result.withDefault ss

                    UpdateProjection proj ->
                        { ss | projectionInput = proj }

                    _ -> ss
