module Views.Elements.Alert exposing (..)

{-| Simple reusable alert element for displaying feedback messages
-}

import Element exposing (Attribute, Element, button, el)
import Element.Attributes as Attributes exposing (paddingLeft, paddingRight, paddingXY, vary, width)
import Styles exposing (Styles, Variations)
import Views.Utils exposing (combineAttributes)


view : List (List (Attribute variation msg)) -> Element Styles variation msg -> Element Styles variation msg
view attributes child =
    el Styles.Alert
        (combineAttributes attributes
            [ paddingXY 20 10 ]
        )
        child



-- HELPERS


attrs : List (Attribute Variations msg) -> List (Attribute Variations msg)
attrs attributes =
    attributes


danger : List (Attribute Variations msg)
danger =
    [ vary Styles.Danger True ]


fill : List (Attribute Variations msg)
fill =
    [ width (Attributes.fill 1.0) ]
