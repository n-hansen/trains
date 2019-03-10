module StockMarket.Render exposing (..)

import Basics.Extra exposing (..)
import Char
import Css exposing (Style)
import Dict exposing (Dict)
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import List
import List.Extra as List
import Maybe
import Maybe.Extra as Maybe
import Murmur3
import Set
import StockMarket as SM exposing (Action(..), Market)
import String
import Tree exposing (Tree)
import Tuple
import Util.Dict as Dict


type alias RenderContext msg =
    { market : Market
    , actionMessage : Action -> msg
    , getColor : String -> {primary : Css.Color, secondary : Css.Color}
    }


renderSpreadsheet : (Action -> msg) -> Market -> Html msg
renderSpreadsheet actionMessage ({playerOrder} as market) =
    let
        colorMap = assignColors market
        ctx =
            { market = market
            , actionMessage = actionMessage
            , getColor = (\name ->
                              case Dict.get name colorMap of
                                  Just c -> c
                                  Nothing -> { primary = Css.hex "#ffffff"
                                             , secondary = Css.hex "#8c8c8c"
                                             }
                         )
            }
    in
        [ playerInfo
        , stockInfo
        ]
        |> List.concatMap ((|>) ctx)
        |> spreadsheetContainer ctx


spreadsheetContainer : RenderContext msg -> List (Html msg) -> Html msg
spreadsheetContainer {market} =
    let
        { companyOrder, playerOrder } = market
        columns =
            Tree.tree "sheet"
                [ Tree.singleton "playerName"
                , Tree.tree "companies" <|
                    List.map Tree.singleton companyOrder
                , Tree.tree "playerInfo"
                    [ Tree.singleton "certificates"
                    , Tree.singleton "playerCash"
                    , Tree.singleton "playerStockValue"
                    , Tree.singleton "netWorth"
                    ]
                ]

        rows =
            Tree.tree "sheet"
                [ Tree.singleton "headings"
                , Tree.tree "players" <|
                    List.map Tree.singleton playerOrder
                , Tree.tree "stockInfo"
                    [ Tree.singleton "bankShares"
                    , Tree.singleton "companyShares"
                    , Tree.singleton "stockPrice"
                    ]
                ]

        gridContainerStyle =
            Attr.css
                [ gridDisplay
                , gridTemplate rows columns
                , Css.cursor Css.pointer
                ]
    in
    Html.div [ gridContainerStyle ]


playerInfo : RenderContext msg -> List (Html msg)
playerInfo { actionMessage, market, getColor } =
    let
        { playerOrder, activePlayer, playerCash } = market
        infoCells =
            playerOrder
                |> List.concatMap
                    (\player ->
                        [ gridCell player
                            "playerName"
                            [ getColor player
                                  |> .primary
                                  |> Css.backgroundColor
                            ]
                            [ SetActivePlayer player
                                  |> actionMessage
                                  |> Events.onClick
                            ]
                            [ Html.text <|
                                  player ++
                                  if Just player == activePlayer then "*" else ""
                            ]
                        , gridCell player
                            "playerCash"
                            [ getColor player
                                  |> .secondary
                                  |> Css.backgroundColor
                            ]
                            []
                            [ Dict.get player playerCash
                                |> Maybe.withDefault 0
                                |> String.fromInt
                                |> Html.text
                            ]
                        , gridCell player
                            "certificates"
                            [ getColor player
                                  |> .secondary
                                  |> Css.backgroundColor
                            ]
                            []
                            [ SM.playerCertificateCount market player
                                |> String.fromInt
                                |> Html.text
                            ]
                        , gridCell player
                            "playerStockValue"
                            [ getColor player
                                  |> .secondary
                                  |> Css.backgroundColor
                            ]
                            []
                            [ SM.playerStockValue market player
                                |> String.fromInt
                                |> Html.text
                            ]
                        , gridCell player
                            "netWorth"
                            [ getColor player
                                  |> .secondary
                                  |> Css.backgroundColor
                            ]
                            []
                            [ SM.playerNetWorth market player
                                |> String.fromInt
                                |> Html.text
                            ]
                        ]
                    )

        headings =
            [ ( "playerName", "Player" )
            , ( "playerCash", "Cash" )
            , ( "certificates", "Certs" )
            , ( "playerStockValue", "Stock" )
            , ( "netWorth", "Net Worth" )
            ]
                |> List.map
                    (\( colName, txt ) ->
                        gridCell "headings"
                            colName
                            []
                            []
                            [ Html.text txt ]
                    )
    in
        headings ++ infoCells


stockInfo : RenderContext msg -> List (Html msg)
stockInfo { actionMessage, market, getColor } =
    let
        { playerOrder, activePlayer, companyOrder, playerShares, presidents, bankShares, shareValues } = market
        activePlayerAction cont event =
            activePlayer
                |> Maybe.map (cont >> actionMessage >> event)
                |> Maybe.toList
        companyInfo =
            companyOrder
                |> List.concatMap
                    (\company ->
                        [ gridCell "headings"
                            company
                            [ getColor company
                                  |> .primary
                                  |> Css.backgroundColor
                            ]
                            []
                            [ Html.text company ]
                        , gridCell "bankShares"
                            company
                            []
                            (activePlayerAction
                                 (\player -> BuyShareFromBank player company)
                                 Events.onClick
                            )
                            [ Dict.get company bankShares
                                |> Maybe.withDefault 0
                                |> String.fromInt
                                |> Html.text
                            ]
                        , gridCell "companyShares"
                            company
                            []
                            (activePlayerAction
                                 (\player -> BuyShareFromCompany player company)
                                 Events.onClick
                            )
                            [ SM.companyShares market company
                                |> String.fromInt
                                |> Html.text
                            ]
                        , gridCell "stockPrice"
                            company
                            []
                            []
                            ( SM.companyShareValue market company
                                  |> Maybe.map ( String.fromInt
                                                     >> Html.text
                                                     >> List.singleton
                                                     >> Html.span []
                                                     >> (\number ->
                                                             [ Html.span
                                                                   [ MoveShareValueLeft company
                                                                         |> actionMessage
                                                                         |> Events.onClick
                                                                   ]
                                                                   [ Html.text "\u{25c0} " ]
                                                             , number
                                                             , Html.span
                                                                   [ MoveShareValueRight company
                                                                         |> actionMessage
                                                                         |> Events.onClick
                                                                   ]
                                                                   [ Html.text " \u{25b6}" ]
                                                             ]
                                                        )
                                               )
                                  |> Maybe.withDefault []
                            )
                        ]
                    )

        stockHeadings =
            [ ( "bankShares", "Market" )
            , ( "companyShares", "Treasury" )
            , ( "stockPrice", "Stock Price" )
            ]
                |> List.map
                    (\( rowName, txt ) ->
                        gridCell rowName
                            "playerName"
                            []
                            []
                            [ Html.text txt ]
                    )

        playerShareInfo =
            companyOrder
                |> List.concatMap
                    (\company ->
                        List.map
                            (\player ->
                                gridCell player
                                    company
                                    [ getColor player
                                          |> .secondary
                                          |> Css.backgroundColor
                                    ]
                                    ( if activePlayer == Just player
                                      then
                                          SellShareToBank player company
                                              |> actionMessage
                                              |> Events.onClick
                                              |> List.singleton
                                      else []
                                    )
                                    [ Dict.get2 player company playerShares
                                        |> Maybe.map String.fromInt
                                        |> Maybe.map
                                            (\cnt ->
                                                if Dict.get company presidents == Just player then
                                                    cnt ++ "*"

                                                else
                                                    cnt
                                            )
                                        |> Maybe.withDefault ""
                                        |> Html.text
                                    ]
                            )
                            playerOrder
                    )
    in
    stockHeadings ++ companyInfo ++ playerShareInfo


-- Styling


assignColors : Market -> Dict String { primary : Css.Color
                                     , secondary : Css.Color
                                     }
assignColors {playerOrder,companyOrder} =
    let
        sortedPlayers = List.sort playerOrder
        sortedCompanies = List.sort companyOrder
        seed = String.join "#" sortedPlayers
                   |> Murmur3.hashString 42
                   |> modBy 9989899
                   |> toFloat
                   |> (*) (360.0 / 9989899.0)
        next x = x + (360 * 0.618033988749895)
                     |> fractionalModBy 360
    in
        sortedPlayers ++ sortedCompanies
            |> List.mapAccuml
               (\hue name ->
                    ( next hue
                    , ( name
                      , { primary = Css.hsl hue 0.9 0.65
                        , secondary = Css.hsl hue 0.85 0.82
                        }
                      )
                    )
               ) seed
            |> Tuple.second
            |> Dict.fromList



-- CSS Grid plumbing


gridDisplay : Style
gridDisplay =
    Css.property "display" "grid"


buildLineSpec : Tree String -> List String
buildLineSpec =
    let
        computeSections =
            Tree.restructure
                identity
                (\label children ->
                    if List.isEmpty children then
                        [ Set.singleton label ]

                    else
                        children |> List.concat |> List.map (Set.insert label)
                )

        makeBegins =
            Set.toList >> List.map sectionBeginLine

        makeEnds =
            Set.toList >> List.map sectionEndLine

        computeLines sections =
            case sections of
                [] ->
                    []

                head :: _ ->
                    makeBegins head :: computeLinesHelper sections

        computeLinesHelper sections =
            case sections of
                [] ->
                    []

                [ last ] ->
                    [ makeEnds last ]

                xs :: ys :: zs ->
                    let
                        beginning =
                            Set.diff ys xs

                        ending =
                            Set.diff xs ys
                    in
                    (makeBegins beginning ++ makeEnds ending) :: computeLinesHelper (ys :: zs)

        buildLineStrings =
            List.map (String.join " " >> (\s -> "[" ++ s ++ "]"))
    in
    computeSections >> computeLines >> buildLineStrings


gridTemplate : Tree String -> Tree String -> Style
gridTemplate rowSections colSections =
    let
        -- TODO maybe don't use auto-spacing?
        lineSpecToAxisSpec =
            String.join " auto "
    in
    Css.batch
        [ buildLineSpec rowSections
            |> lineSpecToAxisSpec
            |> Css.property "grid-template-rows"
        , buildLineSpec colSections
            |> lineSpecToAxisSpec
            |> Css.property "grid-template-columns"
        ]


sanitizeSectionName : String -> String
sanitizeSectionName =
    String.map <|
        \c ->
            if Char.isAlphaNum c then
                c

            else
                '_'


sectionBeginLine : String -> String
sectionBeginLine =
    sanitizeSectionName >> (++) "begin-"


sectionEndLine : String -> String
sectionEndLine =
    sanitizeSectionName >> (++) "end-"


gridCell :
    String
    -> String
    -> List Style
    -> List (Attribute msg)
    -> List (Html msg)
    -> Html msg
gridCell row col styles attrs children =
    Html.div
        (Attr.css
            ([ Css.property "grid-row" (sectionBeginLine row ++ " / " ++ sectionEndLine row)
             , Css.property "grid-column" (sectionBeginLine col ++ " / " ++ sectionEndLine col)
             , Css.border3 (Css.px 1) Css.solid (Css.rgb 0 0 0)
             , Css.margin4 (Css.px 0) (Css.px -1) (Css.px -1) (Css.px 0)
             , Css.padding (Css.px 2)
             ]
                ++ styles
            )
            :: attrs
        )
        children
