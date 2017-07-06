module Util exposing (..)

{-| Takes two lists and returns all possible combinations of their elements,
i.e. their cartesian product.
-}


combinations : List a -> List b -> List ( a, b )
combinations xs ys =
    List.concat (List.map (\x -> List.map (\y -> ( x, y )) ys) xs)
