module Scenes.Editor exposing (ExternalMsg, Model, Msg, init, subscriptions, update, view)

{-| Editor scene - responsible for manipulating the provided image and downloading it
-}

import Canvas exposing (Canvas, DrawOp(..), Error, Point, Size)
import Color exposing (Color)
import Element exposing (Element, column, el, empty, row, text)
import Element.Attributes exposing (center, clip, fill, height, inlineStyle, px, spacing, vary, verticalCenter, width)
import Element.Events
import Maybe exposing (Maybe(Just, Nothing))
import Mouse
import Navigation
import Ports
import Route
import Styles exposing (Styles, Variations)
import Task
import Views.Elements.Alert as Alert
import Views.Elements.Button as Button
import Views.Elements.Events as Events exposing (onMouseDown, onMouseMove, onMouseUp)


type Msg
    = PositionChange Events.Position
    | DragStart Events.Position
    | DragEnd Mouse.Position
    | ZoomIn
    | ZoomOut
    | DownloadImage
    | ImageLoaded (Result Error Canvas)
    | NavigateHome


type ExternalMsg
    = NoOp


type alias Zoom =
    Float


{-| Scene Model. Note, canvas is either nothing or a canvas with image already drawn
-}
type alias Model =
    { drag : Maybe Drag
    , position : Position
    , zoom : Zoom
    , outputSize : Int
    , canvas : Maybe Canvas
    , hasImageLoadFailed : Bool
    }


type alias Drag =
    { start : Position
    , current : Position
    }


type alias Position =
    { x : Int, y : Int }


init : Maybe String -> ( Model, Cmd Msg )
init url =
    let
        model =
            { drag = Nothing
            , position = initPosition
            , zoom = 1.0
            , canvas = Nothing
            , hasImageLoadFailed = False
            , outputSize = 128
            }

        cmd =
            case url of
                Just url ->
                    loadImage url

                option2 ->
                    Cmd.none
    in
    ( model, cmd )


initPosition : { x : Int, y : Int }
initPosition =
    { x = 0, y = 0 }


initDrag : { start : Position, current : Position }
initDrag =
    { start = initPosition, current = initPosition }


setPosition : Position -> Model -> Model
setPosition position model =
    { model | position = position }


setDrag : Maybe Drag -> Model -> Model
setDrag drag model =
    { model | drag = drag }


setDragStart : Int -> Int -> Model -> Model
setDragStart x y model =
    let
        drag =
            case model.drag of
                Just drag ->
                    drag

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


setDragCurrent : Int -> Int -> Model -> Model
setDragCurrent x y model =
    let
        drag =
            case model.drag of
                Just drag ->
                    drag

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


setZoom : Float -> Model -> Model
setZoom zoom model =
    { model | zoom = zoom }


setCanvas : Maybe Canvas -> Model -> Model
setCanvas canvas model =
    { model | canvas = canvas }


setHasImageLoadFailed : Bool -> Model -> Model
setHasImageLoadFailed hasImageLoadFailed model =
    { model | hasImageLoadFailed = hasImageLoadFailed }


isDragging : Model -> Bool
isDragging model =
    case model.drag of
        Just _ ->
            True

        Nothing ->
            False



-- UPDATE


loadImage : String -> Cmd Msg
loadImage url =
    Task.attempt
        ImageLoaded
        (Canvas.loadImage url)


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        PositionChange position ->
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
                            { x = model.position.x - (drag.start.x - drag.current.x)
                            , y = model.position.y - (drag.start.y - drag.current.y)
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
                        |> setZoom (model.zoom + 0.2)
            in
            ( newModel, Cmd.none )

        ZoomOut ->
            let
                newModel =
                    model
                        |> setZoom (model.zoom - 0.2)
            in
            ( newModel, Cmd.none )

        DownloadImage ->
            let
                outputMimetype =
                    "image/png"

                outputQuality =
                    0.92

                imageDataUrl =
                    case model.canvas of
                        Just canvas ->
                            Canvas.toDataUrl outputMimetype outputQuality <|
                                drawCanvas model.outputSize canvas model

                        option2 ->
                            ""

                newCmd =
                    Ports.downloadDataUrl imageDataUrl
            in
            ( model, newCmd )

        ImageLoaded (Ok canvas) ->
            let
                newModel =
                    model
                        |> setCanvas (Just canvas)
            in
            ( newModel, Cmd.none )

        ImageLoaded (Err canvas) ->
            ( setHasImageLoadFailed True model, Cmd.none )

        NavigateHome ->
            let
                newCmd =
                    Navigation.newUrl (Route.routeToString Route.Home)
            in
            ( model, newCmd )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.ups DragEnd ]



-- VIEW


view : String -> Model -> Element Styles Styles.Variations Msg
view url model =
    let
        rows =
            case model.hasImageLoadFailed of
                True ->
                    viewImageLoadFailMessage

                False ->
                    viewMainEditor url model
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


viewMainEditor : String -> Model -> List (Element Styles Variations Msg)
viewMainEditor url model =
    [ viewPageTitle
    , el Styles.EditorContainer
        [ width (px 300)
        , height (px 300)
        , clip
        , onMouseMove PositionChange
        , onMouseDown DragStart
        ]
        (row Styles.None
            []
            [ viewImageOverlay
            , viewImage url model
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


viewImage : String -> Model -> Element Styles Styles.Variations Msg
viewImage url model =
    case model.canvas of
        Just canvas ->
            viewImageCanvas canvas model

        Nothing ->
            viewImagePlaceholder


viewImagePlaceholder : Element style variation msg
viewImagePlaceholder =
    empty


viewImageCanvas : Canvas -> Model -> Element Styles Styles.Variations Msg
viewImageCanvas canvas model =
    let
        newCanvas =
            drawCanvas 300 canvas model
    in
    Element.html <|
        Canvas.toHtml [] newCanvas


viewControls : Model -> Element Styles Styles.Variations Msg
viewControls model =
    let
        isCanvasLoaded =
            case model.canvas of
                Just canvas ->
                    True

                Nothing ->
                    False
    in
    column Styles.None
        [ spacing 20, width (fill 1.0) ]
        [ row Styles.None
            [ spacing 20 ]
            [ Button.view
                [ Button.primary
                , Button.fill
                , Button.onClick ZoomOut
                ]
                (text "Zoom Out")
            , Button.view
                [ Button.primary
                , Button.fill
                , Button.onClick ZoomIn
                ]
                (text "Zoom In")
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


drawCanvas : Int -> Canvas -> Model -> Canvas
drawCanvas outputSize canvas model =
    let
        containerSize =
            300

        ( dragOffsetX, dragOffsetY ) =
            case model.drag of
                Just drag ->
                    ( drag.current.x - drag.start.x
                    , drag.current.y - drag.start.y
                    )

                Nothing ->
                    ( 0, 0 )

        scaleExtraSize =
            ((containerSize * model.zoom) - containerSize) / 2

        srcX =
            toFloat (1 - (model.position.x + dragOffsetX))

        srcY =
            toFloat (1 - (model.position.y + dragOffsetY))

        srcSize =
            round (containerSize - scaleExtraSize)

        setBackground =
            Canvas.FillStyle (Color.rgba 0 0 0 0.0)

        drawImage =
            Canvas.CropScaled
                (Point srcX srcY)
                (Size srcSize srcSize)
                (Point 0 0)
                (Size outputSize outputSize)
                |> DrawImage
                    canvas

        drawOp =
            Canvas.batch [ drawImage ]
    in
    Canvas.draw drawOp (Canvas.initialize (Size outputSize outputSize))
