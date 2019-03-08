module StockMarket exposing (..)

import Dict exposing (Dict)
import Maybe
import Result
import Result.Extra as Result

-- Types

type alias PlayerName = String
type alias CompanyName = String

type alias Market =
    { playerOrder : List PlayerName
    , companyOrder : List CompanyName
    , playerCash : Dict PlayerName Int
    , playerShares : Dict PlayerName (Dict CompanyName Int)
    , presidents : Dict CompanyName PlayerName
    , marketShares : Dict CompanyName Int
    , companyCash : Dict CompanyName Int
    , shareValues : Dict CompanyName Int
    , bank : Int
    , totalShares : Int
    }

type alias Res a = Result String a

-- Computed attributes

playerCertificateCount : Market -> PlayerName -> Int
playerCertificateCount {presidents, playerShares} player =
    Dict.get player playerShares
        |> Maybe.map (Dict.values
                          >> List.sum
                          >> \shareCount -> shareCount - (Dict.values presidents
                                                         |> List.filter (\p -> p == player)
                                                         |> List.length
                                                         )
                     )
        |> Maybe.withDefault 0

playerStockValue : Market -> PlayerName -> Int
playerStockValue {playerShares, shareValues} player =
    Dict.get player playerShares
        |> Maybe.map ( Dict.toList
                           >> List.map (\(company,shares) ->
                                            Dict.get company shareValues
                                            |> Maybe.map (\val -> val * shares)
                                            |> Maybe.withDefault 0
                                       )
                           >> List.sum
                     )
        |> Maybe.withDefault 0

playerNetWorth : Market -> PlayerName -> Int
playerNetWorth ({playerCash} as market) player =
    playerStockValue market player + ( Dict.get player playerCash
                                     |> Maybe.withDefault 0
                                     )

