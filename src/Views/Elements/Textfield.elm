module Views.Elements.Textfield exposing (..)

{-| Simple reusable textfield element
-}

import Element exposing (Element, Attribute, inputText)
import Element.Attributes as Attributes
import Element.Events as Events
import Styles
import Views.Utils exposing (combineAttributes)


view :
    List (List (Attribute variation msg))
    -> String
    -> Element Styles.Styles variation msg
view attributes text =
    inputText Styles.Textfield
        (combineAttributes attributes
            [ Attributes.padding 10 ]
        )
        text



-- HELPERS


placeholder : String -> List (Attribute variation msg)
placeholder string =
    [ Attributes.placeholder string ]


onInput : (String -> msg) -> List (Attribute variation msg)
onInput handler =
    [ Events.onInput handler ]
