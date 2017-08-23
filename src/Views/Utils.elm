module Views.Utils exposing (..)

import Element exposing (Element, Attribute)


combineAttributes :
    List (List (Attribute variation msg))
    -> List (Attribute variation msg)
    -> List (Attribute variation msg)
combineAttributes nestedAttributes attributes =
    List.concat
        [ attributes
        , (List.concat nestedAttributes)
        ]
