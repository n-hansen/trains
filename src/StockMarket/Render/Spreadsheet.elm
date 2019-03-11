module StockMarket.Render.Spreadsheet exposing (renderSpreadsheet,buildLineSpec)

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
import Set
import StockMarket as SM exposing (Action(..), Market, Projection)
import StockMarket.Render.Types exposing (..)
import String
import Tree exposing (Tree)
import Tuple
import Util.Color as Color
import Util.Dict as Dict


renderSpreadsheet : RenderContext msg -> Html msg
renderSpreadsheet ctx =
    [ playerInfo
    , stockInfo
    , instructions
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
                , Css.marginBottom <| Css.px 30
                ]
    in
        Html.div [ gridContainerStyle ]


instructions : a -> List (Html msg)
instructions _ =
    [ gridCell "stockInfo" "playerInfo"
          [ Css.width <| Css.px 320
          , Css.fontSize Css.small
          , Css.padding4 (Css.px 8) (Css.px 0) (Css.px 2) (Css.px 15)
          , Css.borderWidth <| Css.px 0
          ]
          []
          [ Html.text <|
                "Buy shares for the active player (*) by clicking on the market and treasury cells. Sell shares by clicking on cells in that player's row."
          ]
    ]
    -- [Html.div
    --      [ Attr.css  [ Css.property "grid-row" (sectionBeginLine "companyShares" ++ " / " ++ sectionEndLine "sheet")
    --                  , Css.property "grid-column" (sectionBeginLine "playerCash"  ++ " / " ++ sectionEndLine "sheet")
    --                  , Css.width <| Css.px 200
    --                  , Css.fontSize Css.small
    --                  ]
    --      ]
    --      [ Html.text <| "Buy shares for the active player (*) by clicking on the market and treasury cells. Sell shares by clicking on cells in that player's row."

    --      ]
    -- ]


playerInfo : RenderContext msg -> List (Html msg)
playerInfo { actionMessage, market, getColor } =
    let
        { playerOrder, activePlayer, playerCash } = market
        infoCells =
            playerOrder
                |> List.concatMap
                    (\player ->
                         let primaryBackground =
                                 getColor player
                                     |> .primary
                                     |> Color.toCssColor
                                     |> Css.backgroundColor
                             secondaryBackground =
                                 getColor player
                                     |> .secondary
                                     |> Color.toCssColor
                                     |> Css.backgroundColor
                         in
                             [ gridCell player
                                 "playerName"
                                 [ primaryBackground ]
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
                                 [ secondaryBackground ]
                                 []
                                 [ Dict.get player playerCash
                                     |> Maybe.withDefault 0
                                     |> String.fromInt
                                     |> Html.text
                                 ]
                             , gridCell player
                                 "certificates"
                                 [ secondaryBackground ]
                                 []
                                 [ SM.playerCertificateCount market player
                                     |> String.fromInt
                                     |> Html.text
                                 ]
                             , gridCell player
                                 "playerStockValue"
                                 [ secondaryBackground ]
                                 []
                                 [ SM.playerStockValue market player
                                     |> String.fromInt
                                     |> Html.text
                                 ]
                             , gridCell player
                                 "netWorth"
                                 [ secondaryBackground ]
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
                                  |> Color.toCssColor
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
                                          |> Color.toCssColor
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
