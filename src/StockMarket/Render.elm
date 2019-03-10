module StockMarket.Render exposing (..)

import Basics.Extra exposing (..)
import Char
import Css exposing (Style)
import Dict
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import List
import List.Extra as List
import Maybe
import Set
import StockMarket as SM exposing (Market)
import String
import Tree exposing (Tree)
import Tuple
import Util.Dict as Dict


renderSpreadsheet : Market -> Html msg
renderSpreadsheet market =
    [ playerInfo
    , stockInfo
    ]
        |> List.concatMap ((|>) market)
        |> spreadsheetContainer market


spreadsheetContainer : Market -> List (Html msg) -> Html msg
spreadsheetContainer { companyOrder, playerOrder } =
    let
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
                ]
    in
    Html.div [ gridContainerStyle ]


playerInfo : Market -> List (Html msg)
playerInfo ({ playerOrder, playerCash } as market) =
    let
        infoCells =
            playerOrder
                |> List.concatMap
                    (\player ->
                        [ Html.div
                            [ Attr.css [ gridLocation player "playerName" ] ]
                            [ Html.text player ]
                        , Html.div
                            [ Attr.css [ gridLocation player "playerCash" ] ]
                            [ Dict.get player playerCash
                                |> Maybe.withDefault 0
                                |> String.fromInt
                                |> Html.text
                            ]
                        , Html.div
                            [ Attr.css [ gridLocation player "certificates" ] ]
                            [ SM.playerCertificateCount market player
                                |> String.fromInt
                                |> Html.text
                            ]
                        , Html.div
                            [ Attr.css [ gridLocation player "playerStockValue" ] ]
                            [ SM.playerStockValue market player
                                |> String.fromInt
                                |> Html.text
                            ]
                        , Html.div
                            [ Attr.css [ gridLocation player "netWorth" ] ]
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
                        Html.div
                            [ Attr.css [ gridLocation "headings" colName ] ]
                            [ Html.text txt ]
                    )
    in
    headings ++ infoCells


stockInfo : Market -> List (Html msg)
stockInfo ({ playerOrder, companyOrder, playerShares, presidents, bankShares, shareValues } as market) =
    let
        companyInfo =
            companyOrder
                |> List.concatMap
                    (\company ->
                        [ Html.div
                            [ Attr.css [ gridLocation "headings" company ] ]
                            [ Html.text company ]
                        , Html.div
                            [ Attr.css [ gridLocation "bankShares" company ] ]
                            [ Dict.get company bankShares
                                |> Maybe.withDefault 0
                                |> String.fromInt
                                |> Html.text
                            ]
                        , Html.div
                            [ Attr.css [ gridLocation "companyShares" company ] ]
                            [ SM.companyShares market company
                                |> String.fromInt
                                |> Html.text
                            ]
                        , Html.div
                            [ Attr.css [ gridLocation "stockPrice" company ] ]
                            [ Dict.get company shareValues
                                |> Maybe.withDefault 0
                                |> String.fromInt
                                |> Html.text
                            ]
                        ]
                    )

        stockHeadings =
            [ ( "bankShares", "Market" )
            , ( "companyShares", "Treasury" )
            , ( "stockPrice", "Stock Price" )
            ]
                |> List.map
                    (\( rowName, txt ) ->
                        Html.div
                            [ Attr.css [ gridLocation rowName "playerName" ] ]
                            [ Html.text txt ]
                    )

        playerShareInfo =
            companyOrder
                |> List.concatMap
                    (\company ->
                        playerOrder
                            |> List.map
                                (\player ->
                                    Html.div
                                        [ Attr.css [ gridLocation player company ] ]
                                        [ Dict.get2 player company playerShares
                                            |> Maybe.map String.fromInt
                                            |> Maybe.withDefault ""
                                            |> Html.text
                                        ]
                                )
                    )
    in
    stockHeadings ++ companyInfo ++ playerShareInfo



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
        lineSpecToAxisSpec =
            String.join " auto "

        -- TODO maybe don't use auto-spacing?
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


gridLocation : String -> String -> Style
gridLocation row column =
    let
        colBegin =
            sectionBeginLine column

        colEnd =
            sectionEndLine column

        rowBegin =
            sectionBeginLine row

        rowEnd =
            sectionEndLine row
    in
    Css.batch
        [ Css.property "grid-row" (rowBegin ++ " / " ++ rowEnd)
        , Css.property "grid-column" (colBegin ++ " / " ++ colEnd)
        ]
