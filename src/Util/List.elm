module Util.List exposing (..)


snoc : a -> List a -> List a
snoc x xs = xs ++ [x]
