module Util.Result exposing (..)

import Result

guard : (a -> Bool) -> err -> Result err a -> Result err a
guard predicate onFail =
    Result.andThen ( \v -> if predicate v then Ok v else Err onFail )


andMapListErr : Result e a -> Result (List e) (a -> b) -> Result (List e) b
andMapListErr next previous =
    case next of
        Ok x ->
            Result.map ((|>) x) previous

        Err err ->
            case previous of
                Err errs ->
                    Err <| err :: errs

                Ok _ ->
                    Err [err]


andMapListConcatErrs : Result (List e) a -> Result (List e) (a -> b) -> Result (List e) b
andMapListConcatErrs next previous =
    case next of
        Ok x ->
            Result.map ((|>) x) previous

        Err errs ->
            case previous of
                Err otherErrs ->
                    Err <| errs ++ otherErrs

                Ok _ ->
                    Err errs
