module StockMarket exposing (..)

import Dict exposing (Dict)
import Maybe exposing (withDefault)

type alias CompanyInfo =
    { cash : Int
    , controlledShares : Int
    , shareValue : Int
    , president : String
    }

type alias PlayerInfo =
    { cash : Int
    , ownedShares : Dict String Int
    }

type alias Market =
    { companies : Dict String CompanyInfo
    , players : Dict String PlayerInfo
    , marketShares : Dict String Int
    , bank : Int
    }

playerCertificateCount : Market -> String -> Int
playerCertificateCount {players, companies} player =
    case Dict.get player players of
        Nothing -> 0
        Just {ownedShares} ->
            ( Dict.values ownedShares |> List.sum ) -
            ( Dict.values companies
            |> List.filter (\{president} -> president == player)
            |> List.length
            )

playerStockValue : Market -> String -> Int
playerStockValue {players,companies} player =
    case Dict.get player players of
        Nothing -> 0
        Just {ownedShares} ->
            Dict.toList ownedShares
                |> List.map (\(company,count) -> count * ( Dict.get company companies
                                                         |> Maybe.map .shareValue
                                                         |> withDefault 0
                                                         )
                            )
                |> List.sum

playerNetWorth : Market -> String -> Int
playerNetWorth ({players} as market) player =
    case Dict.get player players of
        Nothing -> 0
        Just {cash} -> cash + playerStockValue market player
