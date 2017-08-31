module Views.Elements.Slider
    exposing
        ( attrs
        , max
        , min
        , onChange
        , step
        , view
        )

{-| Simple reusable slider element
-}

import Element exposing (Attribute, Element, el, empty, node)
import Element.Attributes as Attributes exposing (fill, type_, value, width)
import Element.Events as Events
import Json.Decode as Json exposing (Decoder, at, string)
import String.Extra exposing (fromFloat)
import Styles exposing (Styles, Variations)
import Views.Utils exposing (combineAttributes)


view : String -> List (List (Attribute variation msg)) -> Element Styles variation msg
view val attributes =
    node "input" <|
        el Styles.Slider
            (combineAttributes attributes
                [ value val
                , type_ "range"
                , width (fill 100)
                ]
            )
            empty



-- HELPERS


attrs : List (Attribute Variations msg) -> List (Attribute Variations msg)
attrs attributes =
    attributes


min : Float -> List (Attribute Variations msg)
min val =
    [ Attributes.min (fromFloat val) ]


max : Float -> List (Attribute Variations msg)
max val =
    [ Attributes.max (fromFloat val) ]


step : Float -> List (Attribute Variations msg)
step val =
    [ Attributes.step (fromFloat val) ]


onChange : (String -> msg) -> List (Attribute variation msg)
onChange msg =
    [ Events.on "change" (Json.map msg changeDecoder) ]


changeDecoder : Decoder String
changeDecoder =
    at [ "target", "value" ] string
