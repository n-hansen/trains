module StockMarket exposing
    ( Action(..)
    , CompanyName
    , Market
    , PlayerName
    , addCompany
    , addPlayer
    , linearShareValueTrack
    , companyShareValue
    , companyShares
    , emptyMarket
    , playerCertificateCount
    , playerNetWorth
    , playerStockValue
    , tryAction
    )

import Dict exposing (Dict)
import List
import List.Extra as List
import Maybe
import Maybe.Extra as Maybe
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


type ShareValueTrack =
    LinearTrack (List (Int, List CompanyName))


type alias Market =
    { playerOrder : List PlayerName
    , activePlayer : Maybe PlayerName
    , companyOrder : List CompanyName
    , playerCash : Dict PlayerName Int
    , playerShares : Dict PlayerName (Dict CompanyName Int)
    , presidents : Dict CompanyName PlayerName
    , bankShares : Dict CompanyName Int
    , companyCash : Dict CompanyName Int
    , shareValues : ShareValueTrack
    , bank : Int
    , totalShares : Int
    , certificateLimit : Int
    }



-- Market construction


emptyMarket : ShareValueTrack -> Int -> Int -> Int -> Market
emptyMarket shareValues initialBank totalShares certificateLimit =
    { playerOrder = []
    , activePlayer = Nothing
    , companyOrder = []
    , playerCash = Dict.empty
    , playerShares = Dict.empty
    , presidents = Dict.empty
    , bankShares = Dict.empty
    , companyCash = Dict.empty
    , shareValues = shareValues
    , bank = initialBank
    , totalShares = totalShares
    , certificateLimit = certificateLimit
    }


addPlayer : PlayerName -> Int -> Market -> Market
addPlayer player cash ({ playerOrder, playerCash, bank, activePlayer } as market) =
    { market
        | playerOrder = playerOrder ++ [ player ]
        , activePlayer = case activePlayer of
                             Just someone -> activePlayer
                             Nothing -> Just player
        , playerCash = Dict.insert player cash playerCash
        , bank = bank - cash
    }


addCompany : CompanyName -> Int -> Market -> Market
addCompany company shareValue ({ companyOrder, companyCash, shareValues } as market) =
    { market
        | companyOrder = companyOrder ++ [ company ]
        , shareValues = insertCompanyShareValue company shareValue shareValues
        , companyCash = Dict.insert company 0 companyCash
    }


linearShareValueTrack : List Int -> ShareValueTrack
linearShareValueTrack =
    List.sort
        >> List.map (\v -> (v,[]))
        >> LinearTrack


insertCompanyShareValue : CompanyName -> Int -> ShareValueTrack -> ShareValueTrack
insertCompanyShareValue company value shareValues =
    case shareValues of
        LinearTrack track ->
            track
                |> List.findIndex (Tuple.first >> (<=) value)
                |> Maybe.withDefault 0
                |> (\ix -> List.updateAt ix (Tuple.mapSecond <| \cs -> cs ++ [company]) track)
                |> LinearTrack



-- Computed reads


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
playerStockValue ({ playerShares } as market) player =
    Dict.get player playerShares
        |> Maybe.map
            (Dict.toList
                >> List.map
                    (\( company, shares ) ->
                        companyShareValue market company
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
companyShares { playerShares, bankShares, totalShares } company =
    totalShares
        - (Dict.get company bankShares |> Maybe.withDefault 0)
        - (Dict.values playerShares
            |> List.concatMap Dict.toList
            |> List.filter (\( c, _ ) -> company == c)
            |> List.map Tuple.second
            |> List.sum
          )


companyShareValue : Market -> CompanyName -> Maybe Int
companyShareValue { shareValues } company =
    case shareValues of
        LinearTrack track ->
            track
                |> List.find (Tuple.second >> List.member company)
                |> Maybe.map Tuple.first


-- Action API
{-
Reifying user actions into values akin to algebraic effects has two distinct advantages:
  1. It allows us to log user-level actions to make traversing the state history easier.
  2. It allows us to provide a simple interface for monadically chaining actions in a way
     that plays nicely with the Elm architecture.
 ~3. It allows us to build a functor out of user actions.~
 -}


type Action
    = Batch (List Action)
    | BuyShareFromBank PlayerName CompanyName
    | BuyShareFromCompany PlayerName CompanyName
    | SellShareToBank PlayerName CompanyName
    | SetActivePlayer PlayerName


tryAction : Action -> Market -> Result String Market
tryAction action market =
    case action of
        Batch actions ->
            List.foldl (tryAction >> Result.andThen) (Ok market) actions

        BuyShareFromBank p c ->
            buyShareFromBank p c market

        BuyShareFromCompany p c ->
            buyShareFromCompany p c market

        SellShareToBank p c ->
            sellShareToBank p c market

        SetActivePlayer p ->
            setActivePlayer p market


-- Action functions
{-
These mutations should be in 1:1 correspondence with reified action values. In general, there
should be very little error-checking at this level: by failing as far down the call stack as
possible, we keep our top-level functions readable and minimize code duplication.
 -}


buyShareFromBank : PlayerName -> CompanyName -> Market -> Result String Market
buyShareFromBank player company market =
    case companyShareValue market company of
        Nothing ->
            Err <| "No share value for company " ++ company ++ "."

        Just shareValue ->
            market
                |> removeBankShare company
                |> Result.andThen (addPlayerShare player company)
                |> Result.andThen (debitPlayer player shareValue)
                |> Result.andThen (creditBank shareValue)


buyShareFromCompany : PlayerName -> CompanyName -> Market -> Result String Market
buyShareFromCompany player company market =
    case companyShareValue market company of
        Nothing ->
            Err <| "No share value for company " ++ company ++ "."

        Just shareValue ->
            market
                |> addPlayerShare player company
                |> Result.guard (\m -> companyShares m company >= 0)
                    ("No shares held by company " ++ company ++ ".")
                |> Result.andThen (debitPlayer player shareValue)
                |> Result.andThen (creditCompany company shareValue)


sellShareToBank : PlayerName -> CompanyName -> Market -> Result String Market
sellShareToBank player company market =
    case companyShareValue market company of
        Nothing ->
            Err <| "No share value for company " ++ company ++ "."

        Just shareValue ->
            market
                |> removePlayerShare player company
                |> Result.andThen (addBankShare company)
                |> Result.andThen (creditPlayer player shareValue)
                |> Result.andThen (debitBank shareValue)


setActivePlayer : PlayerName -> Market -> Result String Market
setActivePlayer player ({ playerOrder, activePlayer } as market) =
    if List.member player playerOrder
    then Ok {market | activePlayer = Just player}
    else Err <| "Player " ++ player ++ " doesn't appear to exist."


-- Lower level mutations


addBankShare : CompanyName -> Market -> Result String Market
addBankShare company ({ totalShares, bankShares } as market) =
    let
        currBankShares =
            Dict.get company bankShares
                |> Maybe.withDefault 0
    in
    if currBankShares + 1 > totalShares // 2 then
        Err "No more than 50% of a company's shares may be on the market."

    else
        Ok
            { market
                | bankShares = Dict.insert company (currBankShares + 1) bankShares
            }


removeBankShare : CompanyName -> Market -> Result String Market
removeBankShare company ({ bankShares } as market) =
    case Dict.get company bankShares of
        Nothing ->
            Err <| "No shares in the stock market for " ++ company ++ "."

        Just 0 ->
            Err <| "No shares in the stock market for " ++ company ++ "."

        Just x ->
            Ok <| { market | bankShares = Dict.insert company (x - 1) bankShares }


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
        |> Result.map (updatePresidency company)


removePlayerShare : PlayerName -> CompanyName -> Market -> Result String Market
removePlayerShare player company ({ playerShares, presidents } as market) =
    if
        Dict.get company presidents
            == Just player
            && Dict.get2 player company playerShares
            == Just 2
    then
        Err "President's share may not be sold."

    else
        case Dict.get2 player company playerShares of
            Nothing ->
                Err <| "No shares of " ++ company ++ " owned by " ++ player ++ "."

            Just 0 ->
                Err <| "No shares of " ++ company ++ " owned by " ++ player ++ "."

            Just x ->
                { market
                    | playerShares =
                        playerShares
                            |> Dict.update player (Maybe.map <| Dict.insert company (x - 1))
                }
                    |> updatePresidency company
                    |> Ok


updatePresidency : CompanyName -> Market -> Market
updatePresidency company ({ presidents, playerShares } as market) =
    { market
        | presidents =
            Dict.toList playerShares
                |> List.map
                    (\( player, shares ) ->
                        Dict.get company shares
                            |> Maybe.map (Tuple.pair player)
                    )
                |> Maybe.values
                |> List.maximumBy Tuple.second
                |> Maybe.map
                    (\( shareLeader, shareCount ) ->
                        case Dict.get company presidents of
                            Nothing ->
                                Dict.insert company shareLeader presidents

                            Just currentPresident ->
                                if
                                    shareCount
                                        > (Dict.get2 currentPresident company playerShares
                                            |> Maybe.withDefault 0
                                          )
                                then
                                    Dict.insert company shareLeader presidents

                                else
                                    presidents
                    )
                |> Maybe.withDefault presidents
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
