module StockMarket.Parser exposing (ParseState,ConfigStatement,blankState,parseAndMaterializeMarket,configurationParser,serialize)

import AssocList as Dict
import Char
import List
import List.Extra as List
import Maybe
import Maybe.Extra as Maybe
import Parser exposing (Parser,(|=),(|.))
import Result
import Result.Extra as Result
import Set
import StockMarket exposing (Market, PlayerName(..), CompanyName(..), ShareValueTrack(..), linearShareValueTrack, insertCompanyShareValue, companyShareValue)
import Tuple
import Util.Result as Result


-- Types


type alias ParseState =
    { playerOrder : List PlayerName
    , activePlayer : Maybe PlayerName
    , companyOrder : List CompanyName
    , playerCash : List (PlayerName, Int)
    , playerShares : List ((PlayerName, CompanyName), Int)
    , presidents : List (CompanyName, PlayerName)
    , bankShares : List (CompanyName, Int)
    , shareValues : List (CompanyName,Int)
    , certificateLimit : Maybe Int
    , bank : Maybe Int
    }


type alias ConfigStatement =
    ParseState -> ParseState


blankState : ParseState
blankState =
    { playerOrder = []
    , activePlayer = Nothing
    , companyOrder = []
    , playerCash = []
    , playerShares = []
    , presidents = []
    , bankShares = []
    , shareValues = []
    , certificateLimit = Nothing
    , bank = Nothing
    }


-- Parsers


configurationParser : Parser (List ConfigStatement)
configurationParser =
    Parser.sequence
        { start = ""
        , end = ""
        , separator = "\n"
        , spaces = spaces
        , trailing = Parser.Optional
        , item = configStatementParser
        }
        |. Parser.end


configStatementParser : Parser ConfigStatement
configStatementParser =
    Parser.oneOf
        [ bankValueParser
        , certificateCountParser
        , playerParser
        , companyParser
        , Parser.succeed identity |. Parser.lineComment "#"
        ]


bankValueParser : Parser ConfigStatement
bankValueParser =
    Parser.succeed
        ( \bankValue st ->
              { st | bank = Just bankValue }
        )
        |. Parser.oneOf
           [ Parser.keyword "bank"
           , Parser.keyword "b"
           ]
        |. assignParser
        |= moneyParser


certificateCountParser : Parser ConfigStatement
certificateCountParser =
    Parser.succeed
        ( \certLimit st ->
              { st | certificateLimit = Just certLimit }
        )
        |. Parser.oneOf
           [ Parser.keyword "certificates"
           , Parser.keyword "certs"
           , Parser.keyword "certificateLimit"
           ]
        |. assignParser
        |= Parser.int


playerParser : Parser ConfigStatement
playerParser =
    Parser.succeed
        ( \player isActive startingCash shares ({playerOrder, playerCash, activePlayer, playerShares, presidents} as st) ->
              { st
                  | playerOrder = player :: playerOrder
                  , playerCash = (player, startingCash) :: playerCash
                  , activePlayer = if isActive then Just player else activePlayer
                  , playerShares = playerShares ++ List.map (\(c,_,s)->((player,c),s)) shares
                  , presidents =
                      shares
                          |> List.filter (\(_,p,_) -> p)
                          |> List.map (\(c,_,_) -> (c,player))
                          |> (++) presidents
              }
        )
        |. Parser.oneOf
           [ Parser.keyword "player"
           , Parser.keyword "p"
           ]
        |. assignParser
        |= Parser.map P nameParser
        |= maybeStar
        |. spaces
        |= moneyParser
        |. spaces
        |= Parser.sequence
           { start = ""
           , separator = "" -- this works, amazingly
           , end = ""
           , spaces = spaces
           , trailing = Parser.Optional
           , item =
               Parser.succeed (\a b c -> (C a,b,c))
                   |= nameParser
                   |= maybeStar
                   |. assignParser
                   |= moneyParser
           }


companyParser : Parser ConfigStatement
companyParser =
    Parser.succeed
        ( \company value shares ({companyOrder, shareValues, bankShares, playerShares, presidents} as st) ->
              let
                  (tShares,pShares) =
                      shares
                          |> List.mapAccuml
                             (\acc (p,_,s) ->
                                  case p of
                                      Nothing -> (acc + s,Nothing)
                                      Just player -> ( acc
                                                     , Just ((player,company), s)
                                                     )
                             ) 0

              in
                  { st
                      | companyOrder = company :: companyOrder
                      , shareValues = (company, value) :: shareValues
                      , playerShares = playerShares ++ Maybe.values pShares
                      , bankShares = if tShares == 0 then bankShares else (company,tShares) :: bankShares
                      , presidents =
                          case List.find (\(_,president,_) -> president) shares of
                              Just (Just player,_,_) -> (company,player) :: presidents
                              _ -> presidents

                  }
        )
        |. Parser.oneOf
           [ Parser.keyword "company"
           , Parser.keyword "c"
           ]
        |. assignParser
        |= Parser.map C nameParser
        |. spaces
        |= moneyParser
        |. spaces
        |= Parser.sequence
           { start = ""
           , separator = "" -- this works, amazingly
           , end = ""
           , spaces = spaces
           , trailing = Parser.Optional
           , item =
               Parser.succeed (\a b c -> (a,b,c))
                   |= Parser.oneOf
                      [ Parser.map (P >> Just) nameParser
                      , Parser.succeed Nothing |. Parser.keyword "bank"
                      , Parser.succeed Nothing |. Parser.keyword "treasury"
                      ]
                   |= maybeStar
                   |. assignParser
                   |= Parser.int
           }




-- Actually spaces and commas.
spaces : Parser ()
spaces = Parser.chompWhile (\c -> c == ' ' || c == ',')


assignParser : Parser ()
assignParser =
    Parser.succeed ()
        |. spaces
        |. Parser.oneOf [ Parser.symbol "=", Parser.symbol ":" ]
        |. spaces


nameParser : Parser String
nameParser =
    let
        forbiddenChars =
            Set.fromList
                [' ','\n','\r','\t',',','$','¥','€','=',':',';','(',')','[',']','{','}','*','#']
        forbiddenWords =
            Set.fromList
                ["bank", "treasury"]
    in
        Parser.variable
            { start = \c -> not <| Char.isDigit c || Set.member c forbiddenChars
            , inner = \c -> not <| Set.member c forbiddenChars
            , reserved = forbiddenWords
            }


moneyParser : Parser Int
moneyParser =
    Parser.succeed identity
        |. Parser.oneOf
           [ Parser.symbol "$"
           , Parser.symbol "¥"
           , Parser.symbol "€"
           , Parser.succeed ()
           ]
        |= Parser.int


maybeStar : Parser Bool
maybeStar =
    Parser.oneOf
        [ Parser.succeed True |. Parser.symbol "*"
        , Parser.succeed False
        ]


-- Parsing/unparsing operations


parseAndMaterializeMarket : String -> Result (List String) Market
parseAndMaterializeMarket  =
    Parser.run configurationParser
        >> Result.mapError (Parser.deadEndsToString >> List.singleton)
        >> Result.map (List.foldl (<|) blankState)
        >> Result.andThen materializeMarket


materializeMarket : ParseState -> Result (List String) Market
materializeMarket st =
    let
        playerOrder = Ok <| List.reverse st.playerOrder
        activePlayer = Ok st.activePlayer
        companyOrder = Ok <| List.reverse st.companyOrder
        playerCash = Ok <| Dict.fromList st.playerCash
        playerShares =
                case
                    st.playerShares
                        |> List.foldl
                           ( \ ((( (P pName as p)
                                 , (C cName as c)), _))
                                 errs ->
                                     if not <| List.member p st.playerOrder
                                     then ("Player " ++ pName ++ " owns stock in " ++ cName ++ " but is missing a declaration.") :: errs
                                     else if not <| List.member c st.companyOrder
                                          then ("Company " ++ cName ++ " is owned by " ++ pName ++ " but is missing a declaration.") :: errs
                                          else errs
                           ) []
                of
                    [] -> Ok <| Dict.fromList st.playerShares
                    errs -> Err errs
        presidents = Ok <| Dict.fromList st.presidents
        bankShares = Ok <| Dict.fromList st.bankShares
        companyCash =
            st.companyOrder
                |> List.map (\c -> (c,0))
                |> Dict.fromList
                |> Ok
        shareValues =
            st.shareValues
                |> List.foldl
                   (\ (company, value) track -> insertCompanyShareValue company value track )

                   (linearShareValueTrack [0,10,20,30,40,50,60,70,80,90,100,112,124,137,150,165,180
                                          ,195,212,230,250,270,295,320,345,375,405,440,475,510,500]
                   )
                |> Ok
        bank = st.bank |> Result.fromMaybe "Please set the amount of money in the bank."
        totalShares = Ok 10
        certificateLimit = st.certificateLimit |> Result.fromMaybe "Please set the certificate limit."
    in
        Ok Market
            |> Result.andMapListErr playerOrder
            |> Result.andMapListErr activePlayer
            |> Result.andMapListErr companyOrder
            |> Result.andMapListErr playerCash
            |> Result.andMapListConcatErrs playerShares
            |> Result.andMapListErr presidents
            |> Result.andMapListErr bankShares
            |> Result.andMapListErr companyCash
            |> Result.andMapListErr shareValues
            |> Result.andMapListErr bank
            |> Result.andMapListErr totalShares
            |> Result.andMapListErr certificateLimit


serialize : Market -> String
serialize ({playerOrder,activePlayer,companyOrder,playerCash,playerShares,presidents,bankShares,companyCash,bank,certificateLimit} as market) =
    let
        bankListing = "b:" ++ String.fromInt bank

        certListing = "certs:" ++ String.fromInt certificateLimit

        playerListings =
            playerOrder
                |> List.map
                   (\(P pName as player) ->
                        "p:"
                            ++ pName
                            ++ (if Just player == activePlayer then "* " else " ")
                            ++ (Dict.get player playerCash
                                    |> Maybe.withDefault 0
                                    |> String.fromInt
                               )
                            ++ " "
                            ++ (playerShares
                                    |> Dict.toList
                                    |> List.filter (\ ((p,_),s) -> p == player && s > 0)
                                    |> List.map (\((_,C cName as c),s) ->
                                                     if Dict.get c presidents == Just player
                                                     then cName ++ "*=" ++ String.fromInt s
                                                     else cName ++ "="  ++ String.fromInt s
                                                )
                                    |> String.join " "
                               )
                   )

        companyListings =
            companyOrder
                |> List.map
                   (\(C cName as company) ->
                        "c:"
                            ++ cName
                            ++ " "
                            ++ (companyShareValue market company
                                    |> Maybe.withDefault 0
                                    |> String.fromInt
                               )
                            ++ " bank="
                            ++ (Dict.get company bankShares
                                    |> Maybe.withDefault 0
                                    |> String.fromInt
                               )
                   )
    in
        [bankListing, certListing]
            ++ companyListings
            ++ playerListings
            |> String.join "\n"
