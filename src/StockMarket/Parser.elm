module StockMarket.Parser exposing (..)

import Char
import Dict
import Maybe
import Parser exposing (Parser,(|=),(|.))
import Set
import StockMarket exposing (Market, PlayerName, CompanyName, ShareValueTrack(..),linearShareValueTrack, insertCompanyShareValue)
import Tuple


-- Types

type alias ParseState =
    { playerOrder : List PlayerName
    , activePlayer : Maybe PlayerName
    , companyOrder : List CompanyName
    , playerCash : List (PlayerName, Int)
    , playerShares : List (PlayerName, CompanyName, Int)
    , presidents : List (CompanyName, PlayerName)
    , bankShares : List (CompanyName, Int)
    , shareValues : List (CompanyName,Int)
    , shareTrack : Maybe ShareValueTrack
    , bank : Maybe Int
    , totalShares : Maybe Int
    , certificateLimit : Maybe Int
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
    , shareTrack = Nothing
    , bank = Nothing
    , totalShares = Nothing
    , certificateLimit = Nothing
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
        , playerParser
        , companyParser
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


playerParser : Parser ConfigStatement
playerParser =
    Parser.succeed
        ( \player isActive startingCash shares ({playerOrder, playerCash, activePlayer, playerShares} as st) ->
              { st
                  | playerOrder = player :: playerOrder
                  , playerCash = (player, startingCash) :: playerCash
                  , activePlayer = if isActive then Just player else activePlayer
                  , playerShares = playerShares ++ List.map (\(c,s)->(player,c,s)) shares
              }
        )
        |. Parser.oneOf
           [ Parser.keyword "player"
           , Parser.keyword "p"
           ]
        |. assignParser
        |= nameParser
        |= Parser.oneOf
           [ Parser.succeed True |. Parser.symbol "*"
           , Parser.succeed False |. spaces
           ]
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
               Parser.succeed Tuple.pair
                   |= nameParser
                   |. assignParser
                   |= moneyParser
           }


companyParser : Parser ConfigStatement
companyParser =
    Parser.succeed
        ( \company value shares ({companyOrder, shareValues, bankShares, playerShares} as st) ->
              let
                  (tShares,pShares) =
                      shares
                          |> List.partition (Tuple.first >> (==) "treasury")
                          |> Tuple.mapFirst (List.map Tuple.second >> List.sum)

              in
                  { st
                      | companyOrder = company :: companyOrder
                      , shareValues = (company, value) :: shareValues
                      , playerShares = playerShares ++ List.map (\(p,s)->(p,company,s)) pShares
                      , bankShares = if tShares == 0 then bankShares else (company,tShares) :: bankShares
                  }
        )
        |. Parser.oneOf
           [ Parser.keyword "company"
           , Parser.keyword "c"
           ]
        |. assignParser
        |= nameParser
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
               Parser.succeed Tuple.pair
                   |= Parser.oneOf
                      [ nameParser
                      , Parser.succeed "treasury" |. Parser.keyword "bank"
                      , Parser.succeed "treasury" |. Parser.keyword "treasury"
                      ]
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
                [' ','\n','\r','\t',',','$','¥','€','=',':','(',')','[',']','{','}','*']
        forbiddenWords =
            Set.fromList
                [ "bank"
                , "treasury"
                , "company"
                , "player"
                ]
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
           , spaces
           ]
        |= Parser.int



-- Parsing operations
