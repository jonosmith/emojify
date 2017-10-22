module Util
    exposing
        ( toTupleWith
        )


toTupleWith : b -> a -> ( a, b )
toTupleWith second first =
    ( first, second )
