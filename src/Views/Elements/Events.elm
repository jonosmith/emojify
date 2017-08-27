module Views.Elements.Events
    exposing
        ( Position
        , onMouseDown
        , onMouseMove
        , onMouseUp
        , positionDecoder
        )

{-| Extra element events not found in the core library
-}

import Element exposing (Attribute)
import Element.Events as Events
import Json.Decode exposing (Decoder, int)
import Json.Decode.Pipeline exposing (decode, required)


type alias Position =
    { clientX : Int
    , clientY : Int
    , layerX : Int
    , layerY : Int
    , pageX : Int
    , pageY : Int
    , screenX : Int
    , screenY : Int
    }


positionDecoder : Decoder Position
positionDecoder =
    decode Position
        |> required "clientX" int
        |> required "clientY" int
        |> required "layerX" int
        |> required "layerY" int
        |> required "pageX" int
        |> required "pageY" int
        |> required "screenX" int
        |> required "screenY" int


onMouseMove : (Position -> msg) -> Attribute variation msg
onMouseMove msg =
    Events.on "mousemove" (Json.Decode.map msg positionDecoder)


onMouseDown : (Position -> msg) -> Attribute variation msg
onMouseDown msg =
    Events.on "mousedown" (Json.Decode.map msg positionDecoder)


onMouseUp : (Position -> msg) -> Attribute variation msg
onMouseUp msg =
    Events.on "mouseup" (Json.Decode.map msg positionDecoder)
