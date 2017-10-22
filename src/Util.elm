module Util
    exposing
        ( appendIf
        , toTupleWith
        )


toTupleWith : b -> a -> ( a, b )
toTupleWith second first =
    ( first, second )


appendIf : Bool -> appendable -> appendable -> appendable
appendIf predicate item existingItems =
    case predicate of
        True ->
            existingItems ++ item

        False ->
            existingItems
