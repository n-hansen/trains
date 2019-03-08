module Util.Dict exposing (..)

import Dict exposing (Dict)
import Maybe

get2 : comparable
     -> comparable2
     -> Dict comparable (Dict comparable2 x)
     -> Maybe x
get2 i j d =
    Dict.get i d
        |> Maybe.andThen (Dict.get j)
