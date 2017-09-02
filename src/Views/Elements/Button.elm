module Views.Elements.Button
    exposing
        ( attrs
        , disabled
        , fill
        , onClick
        , primary
        , secondary
        , view
        , wide
        )

{-| Simple reusable button element
-}

import Element exposing (Attribute, Element, button, el)
import Element.Attributes as Attributes exposing (paddingLeft, paddingRight, paddingXY, vary, width)
import Element.Events as Events
import Styles exposing (Styles, Variations)
import Views.Utils exposing (combineAttributes)


view : List (List (Attribute variation msg)) -> Element Styles variation msg -> Element Styles variation msg
view attributes child =
    button <|
        el Styles.Button
            (combineAttributes attributes
                [ paddingXY 20 10 ]
            )
            child



-- HELPERS


attrs : List (Attribute Variations msg) -> List (Attribute Variations msg)
attrs attributes =
    attributes


primary : List (Attribute Variations msg)
primary =
    [ vary Styles.Primary True ]


secondary : List (Attribute Variations msg)
secondary =
    [ vary Styles.Secondary True ]


wide : List (Attribute Variations msg)
wide =
    [ paddingLeft 40
    , paddingRight 40
    ]


fill : List (Attribute Variations msg)
fill =
    [ width (Attributes.fill 1.0) ]


disabled : List (Attribute Variations msg)
disabled =
    [ Attributes.disabled True
    , vary Styles.Disabled True
    ]


onClick : msg -> List (Attribute variation msg)
onClick msg =
    [ Events.onClick msg ]
