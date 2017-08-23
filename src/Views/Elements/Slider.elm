module Views.Elements.Alert exposing (..)

{-| Simple reusable slider element
-}

import Element exposing (Attribute, Element, el, empty, node)
import Element.Attributes as Attributes exposing (max, min, step, value)
import Styles exposing (Styles, Variations)
import Views.Utils exposing (combineAttributes)


view : String -> List (List (Attribute variation msg)) -> Element Styles variation msg -> Element Styles variation msg
view val attributes child =
    node "range" <|
        el Styles.Slider
            (combineAttributes attributes
                [ Attributes.value val ]
            )
            empty



-- HELPERS


attrs : List (Attribute Variations msg) -> List (Attribute Variations msg)
attrs attributes =
    attributes


min : String -> List (Attribute Variations msg)
min val =
    [ Attributes.min val ]


max : String -> List (Attribute Variations msg)
max val =
    [ Attributes.max val ]


step : String -> List (Attribute Variations msg)
step val =
    [ Attributes.step val ]
