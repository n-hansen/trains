module Main exposing (main)

import AssocList as Dict
import Browser
import Browser.Navigation as Browser
import Debug
import Html.Styled as Html exposing (Html)
import Maybe
import Maybe.Extra as Maybe
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Result
import StockMarket as SM exposing (Action(..),PlayerName(..),CompanyName(..))
import StockMarket.Render as SM
import StockMarket.Render.Configure as Cfg
import StockMarket.Render.Types as SM
import StockMarket.Parser as SM
import Url exposing (Url)


main =
    Browser.application { init = init
                        , view = view >> Html.toUnstyled >> \b -> {title="Trains", body = [b]}
                        , update = update
                        , subscriptions = always Sub.none
                        , onUrlRequest = always VoidMsg
                        , onUrlChange = always VoidMsg
                        }


type Model =
    Spreadsheet { market : SM.Market
                , projectionInput : SM.ProjectionInput
                }
    | Configuration
      { state : Cfg.State
      , url : Url
      , key : Browser.Key
      }


init : () -> Url -> Browser.Key -> (Model, Cmd.Cmd msg)
init flags url key =
    case
        url.query
            |> Maybe.andThen Url.percentDecode
            |> Maybe.andThen (SM.parseAndMaterializeMarket >> Result.toMaybe)
    of
        Just market ->
            ( Spreadsheet { market = market, projectionInput = Dict.empty }
            , Cmd.none
            )

        Nothing ->
            ( Configuration { state = Cfg.blankState, url = url, key = key }
            , Cmd.none
            )


type Message = SubmitConfiguration SM.Market
             | UpdateConfiguration Cfg.State
             | MarketAction SM.Action
             | UpdateProjection SM.ProjectionInput
             | VoidMsg


view model =
    case model of
        Configuration cfg ->
            Cfg.configurationInput UpdateConfiguration SubmitConfiguration cfg.state

        Spreadsheet {market, projectionInput} ->
            SM.createRenderContext
                MarketAction
                UpdateProjection
                market
                projectionInput
                    |> SM.renderMarket


update message model =
    case model of
        Configuration ({url} as cfg) ->
            case message of
                UpdateConfiguration newCfg ->
                    ( Configuration { cfg | state = newCfg } , Cmd.none )

                SubmitConfiguration mkt ->
                    ( Spreadsheet { market = mkt
                                  , projectionInput = Dict.empty
                                  }
                    , SM.serialize mkt
                        |> Url.percentEncode
                        |> (\q -> {url | query = Just q})
                        |> Url.toString
                        |> Browser.pushUrl cfg.key
                    )

                _ -> ( model, Cmd.none )

        Spreadsheet ({market, projectionInput} as ss) ->
            ( Spreadsheet <|
                  case message of
                      MarketAction axn ->
                          market
                              |> SM.tryAction axn
                              |> Result.map (\m -> { ss | market = m })
                              -- |> Result.mapError (Debug.log ">>")
                              |> Result.withDefault ss

                      UpdateProjection proj ->
                          { ss | projectionInput = proj }

                      _ -> ss
            , Cmd.none
            )
