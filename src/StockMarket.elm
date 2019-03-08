module StockMarket exposing (CompanyName, Market, PlayerName, playerCertificateCount, playerStockValue, playerNetWorth, companyShares, buyShareFromMarket, buyShareFromCompany)

import Dict exposing (Dict)
import List
import List.Extra as List
import Maybe
import Result
import Result.Extra as Result
import Tuple
import Util.Dict as Dict
import Util.Result as Result



-- Types


type alias PlayerName =
    String


type alias CompanyName =
    String


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
    , certificateLimit : Int
    }



-- Computed attributes


playerCertificateCount : Market -> PlayerName -> Int
playerCertificateCount { presidents, playerShares } player =
    Dict.get player playerShares
        |> Maybe.map
            (Dict.values
                >> List.sum
                >> (\shareCount ->
                        shareCount
                            - (Dict.values presidents
                                |> List.filter (\p -> p == player)
                                |> List.length
                              )
                   )
            )
        |> Maybe.withDefault 0


playerStockValue : Market -> PlayerName -> Int
playerStockValue { playerShares, shareValues } player =
    Dict.get player playerShares
        |> Maybe.map
            (Dict.toList
                >> List.map
                    (\( company, shares ) ->
                        Dict.get company shareValues
                            |> Maybe.map (\val -> val * shares)
                            |> Maybe.withDefault 0
                    )
                >> List.sum
            )
        |> Maybe.withDefault 0


playerNetWorth : Market -> PlayerName -> Int
playerNetWorth ({ playerCash } as market) player =
    playerStockValue market player
        + (Dict.get player playerCash
            |> Maybe.withDefault 0
          )


companyShares : Market -> CompanyName -> Int
companyShares { playerShares, marketShares, totalShares } company =
    totalShares
        - (Dict.get company marketShares |> Maybe.withDefault 0)
        - (Dict.values playerShares
            |> List.concatMap Dict.toList
            |> List.filter (\( c, _ ) -> company == c)
            |> List.map Tuple.second
            |> List.sum
          )



-- Mutations


buyShareFromMarket : PlayerName -> CompanyName -> Market -> Result String Market
buyShareFromMarket player company ({ shareValues } as market) =
    case Dict.get company shareValues of
        Nothing ->
            Err <| "No share value for company " ++ company ++ "."

        Just shareValue ->
            market
                |> removeMarketShare company
                |> Result.andThen (addPlayerShare player company)
                |> Result.andThen (debitPlayer player shareValue)
                |> Result.andThen (creditBank shareValue)


buyShareFromCompany : PlayerName -> CompanyName -> Market -> Result String Market
buyShareFromCompany player company ({ shareValues } as market) =
    case Dict.get company shareValues of
        Nothing ->
            Err <| "No share value for company " ++ company ++ "."

        Just shareValue ->
            market
                |> addPlayerShare player company
                |> Result.guard (\m -> companyShares m company >= 0)
                   ("No shares held by company " ++ company ++ ".")
                |> Result.andThen (debitPlayer player shareValue)
                |> Result.andThen (creditCompany company shareValue)


removeMarketShare : CompanyName -> Market -> Result String Market
removeMarketShare company ({ marketShares } as market) =
    case Dict.get company marketShares of
        Nothing ->
            Err <| "No shares in the stock market for " ++ company ++ "."

        Just 0 ->
            Err <| "No shares in the stock market for " ++ company ++ "."

        Just x ->
            Ok <| { market | marketShares = Dict.insert company (x - 1) marketShares }


addPlayerShare : PlayerName -> CompanyName -> Market -> Result String Market
addPlayerShare player company ({ playerShares, certificateLimit } as market) =
    { market
        | playerShares =
            playerShares
                |> Dict.update player
                    (Maybe.withDefault (Dict.singleton company 0)
                        >> Dict.update company
                            (Maybe.withDefault 0
                                >> (\x -> Just <| x + 1)
                            )
                        >> Just
                    )
    }
        |> Ok
        |> Result.guard (\m -> playerCertificateCount m player <= certificateLimit)
            ("Player " ++ player ++ " already at certificate limit.")
        |> Result.map updatePresidency


updatePresidency : Market -> Market
updatePresidency ({ presidents, playerShares } as market) =
    { market
        | presidents =
            presidents
                |> Dict.map
                    (\company president ->
                        let
                            presidentShares =
                                Dict.get2 president company playerShares
                                    |> Maybe.withDefault 0

                            ( controller, controllingShares ) =
                                Dict.toList playerShares
                                    |> List.map
                                        (Tuple.mapSecond
                                            (Dict.get company
                                                >> Maybe.withDefault 0
                                            )
                                        )
                                    |> List.maximumBy Tuple.second
                                    |> Maybe.withDefault ( president, presidentShares )
                        in
                        if controllingShares > presidentShares then
                            controller

                        else
                            president
                    )
    }


creditPlayer : PlayerName -> Int -> Market -> Result String Market
creditPlayer player amount ({ playerCash } as market) =
    if amount < 0 then
        Err "Negative transaction amounts are forbidden"

    else
        Dict.get player playerCash
            |> Result.fromMaybe ("No cash entry for player " ++ player ++ ".")
            |> Result.map
                (\currCash ->
                    { market
                        | playerCash = Dict.insert player (currCash + amount) playerCash
                    }
                )


debitPlayer : PlayerName -> Int -> Market -> Result String Market
debitPlayer player amount ({ playerCash } as market) =
    if amount < 0 then
        Err "Negative transaction amounts are forbidden."

    else
        Dict.get player playerCash
            |> Result.fromMaybe ("No cash entry for player " ++ player ++ ".")
            |> Result.guard (\x -> x >= amount) ("Player " ++ player ++ " doesn't have enough cash.")
            |> Result.map
                (\currCash ->
                    { market
                        | playerCash = Dict.insert player (currCash - amount) playerCash
                    }
                )


creditCompany : CompanyName -> Int -> Market -> Result String Market
creditCompany company amount ({ companyCash } as market) =
    if amount < 0 then
        Err "Negative transaction amounts are forbidden."

    else
        Dict.get company companyCash
            |> Result.fromMaybe ("No cash entry for company " ++ company ++ ".")
            |> Result.map
                (\currCash ->
                    { market
                        | companyCash = Dict.insert company (currCash + amount) companyCash
                    }
                )


debitCompany : CompanyName -> Int -> Market -> Result String Market
debitCompany company amount ({ companyCash } as market) =
    if amount < 0 then
        Err "Negative transaction amounts are forbidden."

    else
        Dict.get company companyCash
            |> Result.fromMaybe ("No cash entry for company " ++ company ++ ".")
            |> Result.guard (\x -> x >= amount) ("Company " ++ company ++ " doesn't have enough cash.")
            |> Result.map
                (\currCash ->
                    { market
                        | companyCash = Dict.insert company (currCash - amount) companyCash
                    }
                )


creditBank : Int -> Market -> Result String Market
creditBank amount ({ bank } as market) =
    if amount < 0 then
        Err "Negative transaction amounts are forbidden."

    else
        Ok { market | bank = bank + amount }


debitBank : Int -> Market -> Result String Market
debitBank amount ({ bank } as market) =
    if amount < 0 then
        Err "Negative transaction amounts are forbidden."

    else
        Ok { market | bank = bank - amount }
