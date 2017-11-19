module Pages.Editor exposing (ExternalMsg(NoOp, ResetApp), Model, Msg, init, subscriptions, update, view)

{-| Editor scene - responsible for manipulating the provided image and downloading it
-}

import Canvas exposing (Canvas, DrawOp(DrawImage, Scale, Translate), Error, Point, Size)
import Element exposing (Element, column, el, empty, row, text)
import Element.Attributes exposing (alignLeft, center, clip, fill, height, inlineStyle, px, spacing, verticalCenter, width)
import Maybe exposing (Maybe(Just, Nothing))
import Mouse
import Ports exposing (ImageDimensions)
import String.Extra exposing (fromFloat)
import Styles exposing (Styles, Variations)
import Task
import Util exposing (appendIf)
import Views.Elements.Alert as Alert
import Views.Elements.Button as Button
import Views.Elements.Events as Events exposing (onMouseDown, onMouseMove)
import Views.Elements.Icon as Icon
import Views.Elements.Slider as Slider


type Msg
    = DownloadImage
    | DragStart Events.Position
    | DragEnd Mouse.Position
    | ImageDimensionsResponse ImageDimensions
    | ImageLoaded (Result Error Canvas)
    | MousePositionChange Events.Position
    | MoveDown
    | MoveLeft
    | MoveRight
    | MoveUp
    | NavigateHome
    | ZoomIn
    | ZoomOut
    | ZoomChange String


type ExternalMsg
    = NoOp
    | ResetApp


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
    , maxZoomMultiplier : Float
    , outputSize : Int
    , outputMimetype : String
    , outputQuality : Float
    }


settings : Settings
settings =
    { containerSize = 300
    , maxZoomMultiplier = 2.0
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


setZoom : Zoom -> Model -> Model
setZoom zoom model =
    { model | zoom = zoom }



-- UPDATE


update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
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
            ( ( newModel, Cmd.none ), NoOp )

        DragStart position ->
            let
                newModel =
                    model
                        |> setDragStart position.layerX position.layerY
                        |> setDragCurrent position.layerX position.layerY
            in
            ( ( newModel, Cmd.none ), NoOp )

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
            in
            ( ( { model | position = newPosition, drag = Nothing }, Cmd.none ), NoOp )

        ZoomIn ->
            ( ( { model | zoom = model.zoom + zoomStep model.imageDimensions }, Cmd.none ), NoOp )

        ZoomOut ->
            ( ( { model | zoom = model.zoom - zoomStep model.imageDimensions }, Cmd.none ), NoOp )

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
            ( ( newModel, Cmd.none ), NoOp )

        MoveDown ->
            ( ( updatePositionByY 1 model, Cmd.none ), NoOp )

        MoveLeft ->
            ( ( updatePositionByX -1 model, Cmd.none ), NoOp )

        MoveRight ->
            ( ( updatePositionByX 1 model, Cmd.none ), NoOp )

        MoveUp ->
            ( ( updatePositionByY -1 model, Cmd.none ), NoOp )

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
            ( ( model, newCmd ), NoOp )

        ImageLoaded (Ok canvas) ->
            let
                newModel =
                    model
                        |> setCanvasWithImage (Just canvas)
            in
            ( ( newModel, Cmd.none ), NoOp )

        ImageLoaded (Err _) ->
            ( ( setHasImageLoadFailed True model, Cmd.none ), NoOp )

        NavigateHome ->
            ( ( model, Cmd.none ), ResetApp )

        ImageDimensionsResponse imageDimensions ->
            let
                newModel =
                    model
                        |> setImageDimensions imageDimensions
                        |> setZoom (defaultZoom imageDimensions)
            in
            ( ( newModel, Cmd.none ), NoOp )


updatePositionByX : Int -> Model -> Model
updatePositionByX x model =
    let
        currentPosition =
            model.position

        newPosition =
            { currentPosition | x = currentPosition.x + x }
    in
    { model | position = newPosition }


updatePositionByY : Int -> Model -> Model
updatePositionByY y model =
    let
        currentPosition =
            model.position

        newPosition =
            { currentPosition | y = currentPosition.y + y }
    in
    { model | position = newPosition }


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
        content =
            case model.hasImageLoadFailed of
                True ->
                    viewImageLoadFailMessage

                False ->
                    viewMainEditor model
    in
    column Styles.None
        [ center, verticalCenter, spacing 20 ]
        [ row Styles.None
            [ alignLeft ]
            [ viewHomeButton
            ]
        , row Styles.None
            []
            [ content
            ]
        ]


viewImageLoadFailMessage : Element Styles Variations Msg
viewImageLoadFailMessage =
    column Styles.None
        [ center, verticalCenter, spacing 20 ]
        [ Alert.view [ Alert.danger ] (text "Failed to load image! Try again with a different one")
        , Button.view [ Button.primary, Button.onClick NavigateHome ] (text "Home")
        ]


viewMainEditor : Model -> Element Styles Variations Msg
viewMainEditor model =
    let
        isDragging =
            case model.drag of
                Nothing ->
                    False

                _ ->
                    True
    in
    row Styles.None
        [ spacing 10 ]
        [ column Styles.None
            [ center, verticalCenter, spacing 20 ]
            [ el Styles.EditorContainer
                ([ width (px (toFloat settings.containerSize))
                 , height (px (toFloat settings.containerSize))
                 , clip
                 , onMouseDown DragStart
                 ]
                    |> appendIf isDragging [ onMouseMove MousePositionChange ]
                )
                (row Styles.None
                    []
                    [ viewImageOverlay
                    , viewImage model
                    ]
                )
            , viewControls model
            ]
        , column Styles.None
            [ spacing 30 ]
            [ viewMoveControls
            ]
        ]


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
        isCanvasLoaded =
            case model.canvasWithImage of
                Just _ ->
                    True

                Nothing ->
                    False
    in
    column Styles.None
        [ spacing 20, width (fill 1.0) ]
        [ viewZoomControls model.zoom model.imageDimensions
        , row Styles.None
            []
            [ Button.withIcon
                [ if isCanvasLoaded then
                    Button.primary
                  else
                    Button.disabled
                , Button.fill
                , Button.onClick DownloadImage
                ]
                "floppy-disk"
                "Download"
            ]
        ]


viewHomeButton : Element Styles Variations Msg
viewHomeButton =
    Button.withIcon [ Button.primary, Button.onClick NavigateHome ] "home3" "Home"


viewMoveControls : Element Styles Variations Msg
viewMoveControls =
    column Styles.None
        [ spacing 5 ]
        [ row Styles.None
            [ center ]
            [ Button.view
                [ Button.secondary
                , Button.icon
                , Button.onClick MoveUp
                ]
                (Icon.view "arrow-up")
            ]
        , row Styles.None
            [ center, spacing 25 ]
            [ Button.view
                [ Button.secondary
                , Button.icon
                , Button.onClick MoveLeft
                ]
                (Icon.view "arrow-left")
            , Button.view
                [ Button.secondary
                , Button.icon
                , Button.onClick MoveRight
                ]
                (Icon.view "arrow-right")
            ]
        , row Styles.None
            [ center ]
            [ Button.view
                [ Button.secondary
                , Button.icon
                , Button.onClick MoveDown
                ]
                (Icon.view "arrow-down")
            ]
        ]


viewZoomControls : Zoom -> ImageDimensions -> Element Styles Variations Msg
viewZoomControls zoom imageDimensions =
    row Styles.None
        [ spacing 20 ]
        [ Button.view [ Button.secondary, Button.small, Button.onClick ZoomOut ] (text "-")
        , Slider.view
            (fromFloat zoom)
            [ Slider.min (zoomStep imageDimensions)
            , Slider.max (maxZoom imageDimensions)
            , Slider.step (zoomStep imageDimensions)
            , Slider.onChange ZoomChange
            ]
        , Button.view [ Button.secondary, Button.small, Button.onClick ZoomIn ] (text "+")
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
        ratioZoomToContainerSize =
            model.zoom / toFloat settings.containerSize

        scaleFactor =
            toFloat settings.outputSize * ratioZoomToContainerSize

        ratioOutputToContainer =
            toFloat settings.outputSize / toFloat settings.containerSize

        position =
            { x = ceiling (toFloat model.position.x * ratioOutputToContainer)
            , y = ceiling (toFloat model.position.y * ratioOutputToContainer)
            }
    in
    drawCanvas
        { mode = Quality
        , drag = Nothing
        , position = position
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
            let
                maxPossibleImageWidth =
                    round (toFloat imageDimensions.width * settings.maxZoomMultiplier)

                maxPossibleImageHeight =
                    round (toFloat imageDimensions.height * settings.maxZoomMultiplier)
            in
            Canvas.draw
                (Canvas.batch
                    [ Scale newScale newScale
                    , Canvas.At
                        (Point 0 0)
                        |> DrawImage
                            imageCanvas
                    ]
                )
                (Canvas.initialize (Size maxPossibleImageWidth maxPossibleImageHeight))
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


maxZoom : ImageDimensions -> Float
maxZoom imageDimensions =
    defaultZoom imageDimensions * settings.maxZoomMultiplier


zoomStep : ImageDimensions -> Float
zoomStep imageDimensions =
    let
        desiredStep =
            0.01

        imageResolutionModifier =
            ratioImageToContainer imageDimensions
    in
    desiredStep * imageResolutionModifier
