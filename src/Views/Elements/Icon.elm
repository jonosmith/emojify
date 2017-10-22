module Views.Elements.Icon
    exposing
        ( view
        )

{-| Simple reusable icon element designed for using Icomoon icons
-}

import Element exposing (Element, el, empty, node)
import Element.Attributes exposing (class)
import Styles exposing (Styles)


view : String -> Element Styles variation msg
view iconName =
    node "i" <|
        el Styles.None
            [ class ("icon-" ++ iconName) ]
            empty
