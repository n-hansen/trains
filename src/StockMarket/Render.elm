module StockMarket.Render exposing (..)


import Basics.Extra exposing (..)
import Css exposing (Style)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import List
import List.Extra as List
import Maybe
import StockMarket as SM exposing (Market)
import Set
import String
import Tree exposing (Tree)

import Debug

renderSpreadsheet : Market -> Html msg
renderSpreadsheet market =
    spreadsheetContainer market
        [ companySpreadsheetGroup market
        , playerSpreadsheetGroup market
        ]


spreadsheetContainer : Market -> List (Html msg) -> Html msg
spreadsheetContainer market children =
    Html.text "TODO: container!"


companySpreadsheetGroup : Market -> Html msg
companySpreadsheetGroup market =
    Html.text "TODO: companies"


playerSpreadsheetGroup : Market -> Html msg
playerSpreadsheetGroup market =
    Html.text "TODO: players"


-- CSS Grid plumbing

gridDisplay : Style
gridDisplay =
    Css.property "display" "grid"


gridTemplate : String -> String -> Style
gridTemplate rowSpec colSpec =
    Css.batch [ Css.property "grid-template-rows" rowSpec
              , Css.property "grid-template-columns" colSpec
              ]


buildLineSpec : Tree String -> List String
buildLineSpec =
    let
        sectionBeginLine = (++) "begin-"
        sectionEndLine = (++) "end-"
        computeSections =
            Tree.restructure
                identity
                (\label children ->
                     if List.isEmpty children
                     then [Set.singleton label]
                     else children |> List.concat |> List.map (Set.insert label))
        makeBegins = Set.toList >> List.map sectionBeginLine
        makeEnds = Set.toList >> List.map sectionEndLine
        computeLines sections =
            case sections of
                [] -> []
                (head::_) -> makeBegins head :: computeLinesHelper sections
        computeLinesHelper sections =
            case sections of
                [] -> []
                [last] -> [makeEnds last]
                (xs::ys::zs) ->
                    let beginning = Set.diff ys xs
                        ending = Set.diff xs ys
                    in (makeBegins beginning ++ makeEnds ending) :: computeLinesHelper (ys::zs)
        buildLineStrings =
            List.map ( String.join " " >> \s -> "["++s++"]" )
    in computeSections >> computeLines >> buildLineStrings


buildGridTemplate :  Tree String -> Tree String -> Style
buildGridTemplate rowSections colSections =
    let lineSpecToAxisSpec = String.join " auto " -- TODO maybe don't use auto-spacing?
    in gridTemplate
        (buildLineSpec rowSections |> lineSpecToAxisSpec)
        (buildLineSpec colSections |> lineSpecToAxisSpec)
