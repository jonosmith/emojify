module Views.Utils exposing (combineAttributes)

import Element exposing (Attribute)


combineAttributes :
    List (List (Attribute variation msg))
    -> List (Attribute variation msg)
    -> List (Attribute variation msg)
combineAttributes nestedAttributes attributes =
    List.concat
        [ attributes
        , List.concat nestedAttributes
        ]
