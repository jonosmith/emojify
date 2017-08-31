port module Ports
    exposing
        ( ImageDimensions
        , downloadDataUrl
        , imageDimensionsRequest
        , imageDimensionsResponse
        )


type alias ImageDimensions =
    { width : Int, height : Int }


{-| Forces a browser download of an image using the given image url
-}
port downloadDataUrl : String -> Cmd msg


{-| Fetches the dimensions for an image with the given url
-}
port imageDimensionsRequest : String -> Cmd msg


{-| The response for the request for image dimensions
-}
port imageDimensionsResponse : (ImageDimensions -> msg) -> Sub msg
