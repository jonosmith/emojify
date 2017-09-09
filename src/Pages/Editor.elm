module Pages.Editor exposing (Model, Msg, init, subscriptions, update, view)

{-| Editor scene - responsible for manipulating the provided image and downloading it
-}

import Canvas exposing (Canvas, DrawOp(DrawImage, Scale, Translate), Error, Point, Size)
import Element exposing (Element, column, el, empty, row, text)
import Element.Attributes exposing (center, clip, fill, height, inlineStyle, px, spacing, vary, verticalCenter, width)
import Element.Events
import Maybe exposing (Maybe(Just, Nothing))
import Mouse
import Navigation
import Ports exposing (ImageDimensions)
import Route
import String.Extra exposing (fromFloat)
import Styles exposing (Styles, Variations)
import Task
import Views.Elements.Alert as Alert
import Views.Elements.Button as Button
import Views.Elements.Events as Events exposing (onMouseDown, onMouseMove)
import Views.Elements.Slider as Slider


type Msg
    = MousePositionChange Events.Position
    | DragStart Events.Position
    | DragEnd Mouse.Position
    | ZoomIn
    | ZoomOut
    | ZoomChange String
    | DownloadImage
    | ImageLoaded (Result Error Canvas)
    | NavigateHome
    | ImageDimensionsResponse ImageDimensions


type CanvasDrawMode
    = Performance
    | Quality



-- MODEL


type alias Model =
    { drag : Maybe Drag
    , position : Position
    , zoom : Zoom
    , imageDimensions : ImageDimensions
    , canvasWithImage : Maybe Canvas
    , hasImageLoadFailed : Bool
    }


type alias Drag =
    { start : Position
    , current : Position
    }


type alias Zoom =
    Float


type alias Position =
    { x : Int
    , y : Int
    }


type alias Settings =
    { containerSize : Int
    , outputSize : Int
    , outputMimetype : String
    , outputQuality : Float
    }


settings : Settings
settings =
    { containerSize = 300
    , outputSize = 128
    , outputMimetype = "image/png"
    , outputQuality = 0.92
    }


init : Maybe String -> ( Model, Cmd Msg )
init maybeUrl =
    let
        model =
            { drag = Nothing
            , position = initPosition
            , zoom = 1.0
            , imageDimensions = initImageDimensions
            , canvasWithImage = Nothing
            , hasImageLoadFailed = False
            }

        cmd =
            case maybeUrl of
                Just url ->
                    Cmd.batch
                        [ loadImage url
                        , Ports.imageDimensionsRequest url
                        ]

                _ ->
                    Cmd.none
    in
    ( model, cmd )


initPosition : Position
initPosition =
    { x = 0, y = 0 }


initImageDimensions : ImageDimensions
initImageDimensions =
    { width = 0, height = 0 }


initDrag : { start : Position, current : Position }
initDrag =
    { start = initPosition, current = initPosition }



-- Setters


setCanvasWithImage : Maybe Canvas -> Model -> Model
setCanvasWithImage canvasWithImage model =
    { model | canvasWithImage = canvasWithImage }


setDrag : Maybe Drag -> Model -> Model
setDrag drag model =
    { model | drag = drag }


setDragCurrent : Int -> Int -> Model -> Model
setDragCurrent x y model =
    let
        drag =
            case model.drag of
                Just dragModel ->
                    dragModel

                Nothing ->
                    initDrag

        current =
            drag.current

        newCurrent =
            { current | x = x, y = y }

        newDrag =
            { drag | current = newCurrent }
    in
    { model | drag = Just newDrag }


setDragStart : Int -> Int -> Model -> Model
setDragStart x y model =
    let
        drag =
            case model.drag of
                Just dragModel ->
                    dragModel

                Nothing ->
                    initDrag

        start =
            drag.start

        newStart =
            { start | x = x, y = y }

        newDrag =
            { drag | start = newStart }
    in
    { model | drag = Just newDrag }


setHasImageLoadFailed : Bool -> Model -> Model
setHasImageLoadFailed hasImageLoadFailed model =
    { model | hasImageLoadFailed = hasImageLoadFailed }


setImageDimensions : ImageDimensions -> Model -> Model
setImageDimensions imageDimensions model =
    { model | imageDimensions = imageDimensions }


setPosition : Position -> Model -> Model
setPosition position model =
    { model | position = position }


setZoom : Zoom -> Model -> Model
setZoom zoom model =
    { model | zoom = zoom }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        MousePositionChange position ->
            let
                newModel =
                    case model.drag of
                        Just drag ->
                            let
                                newDragCurrent =
                                    { x = position.layerX, y = position.layerY }

                                newDrag =
                                    { drag | current = newDragCurrent }
                            in
                            model
                                |> setDrag (Just newDrag)

                        Nothing ->
                            model
            in
            ( newModel, Cmd.none )

        DragStart position ->
            let
                newModel =
                    model
                        |> setDragStart position.layerX position.layerY
                        |> setDragCurrent position.layerX position.layerY
            in
            ( newModel, Cmd.none )

        DragEnd _ ->
            let
                newPosition =
                    case model.drag of
                        Just drag ->
                            let
                                xDragOffset =
                                    drag.start.x - drag.current.x

                                yDragOffset =
                                    drag.start.y - drag.current.y
                            in
                            { x = model.position.x - xDragOffset
                            , y = model.position.y - yDragOffset
                            }

                        Nothing ->
                            model.position

                newModel =
                    model
                        |> setPosition newPosition
                        |> setDrag Nothing
            in
            ( newModel, Cmd.none )

        ZoomIn ->
            let
                newModel =
                    model
                        |> setZoom (model.zoom + zoomStep model.imageDimensions)
            in
            ( newModel, Cmd.none )

        ZoomOut ->
            let
                newModel =
                    model
                        |> setZoom (model.zoom - zoomStep model.imageDimensions)
            in
            ( newModel, Cmd.none )

        ZoomChange newZoom ->
            let
                convertedResult =
                    String.toFloat newZoom

                newModel =
                    case convertedResult of
                        Ok zoomAsFloat ->
                            model
                                |> setZoom zoomAsFloat

                        _ ->
                            model
            in
            ( newModel, Cmd.none )

        DownloadImage ->
            let
                imageDataUrl =
                    case model.canvasWithImage of
                        Just canvas ->
                            Canvas.toDataUrl settings.outputMimetype settings.outputQuality <|
                                drawDownloadCanvas model canvas

                        _ ->
                            ""

                newCmd =
                    Ports.downloadDataUrl imageDataUrl
            in
            ( model, newCmd )

        ImageLoaded (Ok canvas) ->
            let
                newModel =
                    model
                        |> setCanvasWithImage (Just canvas)
            in
            ( newModel, Cmd.none )

        ImageLoaded (Err _) ->
            ( setHasImageLoadFailed True model, Cmd.none )

        NavigateHome ->
            let
                newCmd =
                    Navigation.newUrl (Route.routeToString Route.Home)
            in
            ( model, newCmd )

        ImageDimensionsResponse imageDimensions ->
            let
                newModel =
                    model
                        |> setImageDimensions imageDimensions
                        |> setZoom (defaultZoom imageDimensions)
            in
            ( newModel, Cmd.none )


loadImage : String -> Cmd Msg
loadImage url =
    Task.attempt
        ImageLoaded
        (Canvas.loadImage url)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Mouse.ups DragEnd
        , Ports.imageDimensionsResponse ImageDimensionsResponse
        ]



-- VIEW


view : Model -> Element Styles Styles.Variations Msg
view model =
    let
        rows =
            case model.hasImageLoadFailed of
                True ->
                    viewImageLoadFailMessage

                False ->
                    viewMainEditor model
    in
    column Styles.None
        [ center
        , verticalCenter
        , spacing 20
        ]
        rows


viewImageLoadFailMessage : List (Element Styles Variations Msg)
viewImageLoadFailMessage =
    [ Alert.view [ Alert.danger ] (text "Failed to load image! Try again with a different one")
    , Button.view [ Button.primary, Button.onClick NavigateHome ] (text "Home")
    ]


viewMainEditor : Model -> List (Element Styles Variations Msg)
viewMainEditor model =
    [ viewPageTitle
    , el Styles.EditorContainer
        [ width (px (toFloat settings.containerSize))
        , height (px (toFloat settings.containerSize))
        , clip
        , onMouseMove MousePositionChange
        , onMouseDown DragStart
        ]
        (row Styles.None
            []
            [ viewImageOverlay
            , viewImage model
            ]
        )
    , viewControls model
    ]


viewPageTitle : Element Styles Variations Msg
viewPageTitle =
    el Styles.PageHeader
        [ Element.Events.onClick NavigateHome
        , vary Styles.PageHeaderLink True
        ]
        (Element.text "Emojify")


viewImageOverlay : Element Styles Styles.Variations Msg
viewImageOverlay =
    el Styles.EditorOverlay
        [ inlineStyle
            [ ( "position", "absolute" )
            , ( "height", "100%" )
            , ( "width", "100%" )
            , ( "z-index", "1" )
            ]
        ]
        empty


viewImage : Model -> Element Styles Styles.Variations Msg
viewImage model =
    case model.canvasWithImage of
        Just canvas ->
            viewImageCanvas canvas model

        Nothing ->
            viewImagePlaceholder


viewImagePlaceholder : Element style variation msg
viewImagePlaceholder =
    empty


viewImageCanvas : Canvas -> Model -> Element Styles Styles.Variations Msg
viewImageCanvas canvasWithImage model =
    let
        newCanvas =
            drawPreviewCanvas model canvasWithImage
    in
    Element.html <|
        Canvas.toHtml [] newCanvas


viewControls : Model -> Element Styles Styles.Variations Msg
viewControls model =
    let
        minZoom =
            zoomStep model.imageDimensions

        maxZoom =
            1.0

        isCanvasLoaded =
            case model.canvasWithImage of
                Just _ ->
                    True

                Nothing ->
                    False
    in
    column Styles.None
        [ spacing 20, width (fill 1.0) ]
        [ row Styles.None
            [ spacing 20 ]
            [ Button.view [ Button.secondary, Button.onClick ZoomOut ] (text "-")
            , Slider.view
                (fromFloat model.zoom)
                [ Slider.min minZoom
                , Slider.max maxZoom
                , Slider.step (zoomStep model.imageDimensions)
                , Slider.onChange ZoomChange
                ]
            , Button.view [ Button.secondary, Button.onClick ZoomIn ] (text "+")
            ]
        , row Styles.None
            []
            [ Button.view
                [ if isCanvasLoaded then
                    Button.primary
                  else
                    Button.disabled
                , Button.fill
                , Button.onClick DownloadImage
                ]
                (text "Download")
            ]
        ]


drawPreviewCanvas : Model -> Canvas -> Canvas
drawPreviewCanvas model canvasWithImage =
    let
        mode =
            case model.drag of
                Just _ ->
                    Performance

                Nothing ->
                    Quality
    in
    drawCanvas
        { mode = mode
        , drag = model.drag
        , position = model.position
        , imageDimensions = model.imageDimensions
        , scaleFactor = model.zoom
        , outputSize = settings.containerSize
        , canvasWithImage = canvasWithImage
        }


drawDownloadCanvas : Model -> Canvas -> Canvas
drawDownloadCanvas model canvasWithImage =
    let
        scaleFactor =
            model.zoom * (toFloat settings.outputSize / toFloat settings.containerSize)
    in
    drawCanvas
        { mode = Quality
        , drag = Nothing
        , position = model.position
        , imageDimensions = model.imageDimensions
        , scaleFactor = scaleFactor
        , outputSize = settings.outputSize
        , canvasWithImage = canvasWithImage
        }


drawCanvas :
    { a
        | canvasWithImage : Canvas
        , drag : Maybe Drag
        , imageDimensions : { height : Int, width : Int }
        , mode : CanvasDrawMode
        , outputSize : Int
        , position : Position
        , scaleFactor : Float
    }
    -> Canvas
drawCanvas { canvasWithImage, drag, imageDimensions, mode, outputSize, position, scaleFactor } =
    let
        offsetImageOp =
            canvasImageOffsetOp drag position

        drawOp =
            case mode of
                Performance ->
                    let
                        scaleCanvasOp =
                            Scale scaleFactor scaleFactor
                    in
                    Canvas.batch [ offsetImageOp, scaleCanvasOp, canvasDrawImageOp canvasWithImage ]

                Quality ->
                    let
                        fullCanvasWithImage =
                            Canvas.draw
                                (Canvas.batch [ canvasDrawImageOp canvasWithImage ])
                                (Canvas.initialize (Size imageDimensions.width imageDimensions.height))

                        scaledImageCanvas =
                            progressivelyScaleImageCanvas scaleFactor 1.0 imageDimensions fullCanvasWithImage
                    in
                    Canvas.batch [ offsetImageOp, canvasDrawImageOp scaledImageCanvas ]
    in
    Canvas.draw
        drawOp
        (Canvas.initialize (Size outputSize outputSize))


canvasDrawImageOp : Canvas -> DrawOp
canvasDrawImageOp canvas =
    Canvas.At
        (Point 0 0)
        |> DrawImage
            canvas


canvasImageOffsetOp : Maybe Drag -> Position -> DrawOp
canvasImageOffsetOp maybeDrag position =
    let
        ( dragOffsetX, dragOffsetY ) =
            case maybeDrag of
                Just drag ->
                    ( drag.current.x - drag.start.x
                    , drag.current.y - drag.start.y
                    )

                Nothing ->
                    ( 0, 0 )

        x =
            toFloat (position.x + dragOffsetX)

        y =
            toFloat (position.y + dragOffsetY)
    in
    Translate (Point x y)


progressivelyScaleImageCanvas : Float -> Float -> ImageDimensions -> Canvas -> Canvas
progressivelyScaleImageCanvas targetScale currentScale imageDimensions imageCanvas =
    let
        isScaleDown =
            targetScale < currentScale

        standardScaleStep =
            case isScaleDown of
                True ->
                    0.5

                False ->
                    2

        isFinal =
            case isScaleDown of
                True ->
                    standardScaleStep * currentScale < targetScale

                False ->
                    standardScaleStep * currentScale > targetScale

        newScale =
            if isFinal == True then
                targetScale / currentScale
            else
                standardScaleStep

        newCurrentScale =
            newScale * currentScale

        scaledImageCanvas =
            Canvas.draw
                (Canvas.batch
                    [ Scale newScale newScale
                    , Canvas.At
                        (Point 0 0)
                        |> DrawImage
                            imageCanvas
                    ]
                )
                (Canvas.initialize (Size imageDimensions.width imageDimensions.height))
    in
    if isFinal == True then
        scaledImageCanvas
    else
        progressivelyScaleImageCanvas targetScale newCurrentScale imageDimensions scaledImageCanvas



-- UTIL


maxImageDimension : ImageDimensions -> Float
maxImageDimension imageDimensions =
    toFloat (max imageDimensions.width imageDimensions.height)


ratioImageToContainer : ImageDimensions -> Float
ratioImageToContainer imageDimensions =
    toFloat settings.containerSize / maxImageDimension imageDimensions


defaultZoom : ImageDimensions -> Float
defaultZoom imageDimensions =
    ratioImageToContainer imageDimensions


zoomStep : ImageDimensions -> Float
zoomStep imageDimensions =
    let
        desiredStep =
            0.01

        imageResolutionModifier =
            ratioImageToContainer imageDimensions
    in
    desiredStep * imageResolutionModifier
