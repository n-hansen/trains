module Util.Result exposing (..)

import Result

guard : (a -> Bool) -> err -> Result err a -> Result err a
guard predicate onFail =
    Result.andThen ( \v -> if predicate v then Ok v else Err onFail )
