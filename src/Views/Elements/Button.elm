module Views.Elements.Button
    exposing
        ( attrs
        , disabled
        , fill
        , icon
        , onClick
        , primary
        , secondary
        , small
        , view
        , wide
        , withIcon
        )

{-| Simple reusable button element
-}

import Element exposing (Attribute, Element, button, el, row, text)
import Element.Attributes as Attributes exposing (center, padding, paddingLeft, paddingRight, paddingXY, spacing, vary, width)
import Element.Events as Events
import Styles exposing (Styles, Variations)
import Views.Elements.Icon as Icon
import Views.Utils exposing (combineAttributes)


view : List (List (Attribute variation msg)) -> Element Styles variation msg -> Element Styles variation msg
view attributes child =
    button <|
        el Styles.Button
            (combineAttributes attributes
                [ paddingXY 20 10 ]
            )
            child


withIcon : List (List (Attribute variation msg)) -> String -> String -> Element Styles variation msg
withIcon attributes iconName buttonText =
    view attributes <|
        row
            Styles.None
            [ center, spacing 10 ]
            [ Icon.view iconName
            , text buttonText
            ]



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


icon : List (Attribute Variations msg)
icon =
    [ padding 4 ]


wide : List (Attribute Variations msg)
wide =
    [ paddingLeft 40, paddingRight 40 ]


small : List (Attribute Variations msg)
small =
    [ paddingLeft 20, paddingRight 20 ]


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
