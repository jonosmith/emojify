module Scenes.Home exposing (..)

{-| Home scene - starting point for selecting an image to edit
-}

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events as Events
import FileReader
    exposing
        ( Error
        , FileContentDataUrl
        , FileRef
        , NativeFile
        , parseDroppedFiles
        , readAsDataUrl
        )
import Json.Decode as Decode exposing (decodeString, decodeValue)
import Styles exposing (Styles)
import Task


type Msg
    = UrlSet String
    | EditImage
    | DragEnter
    | DragLeave
    | FileUpload (List NativeFile)
    | FileData (Result Error FileContentDataUrl)


type ExternalMsg
    = NoOp
    | ImageSelected String


type DragState
    = Normal
    | Hovering



-- MODEL


type alias Model =
    { url : String, dragState : DragState }


init : Model
init =
    { url = "", dragState = Normal }


setUrl : String -> Model -> Model
setUrl url model =
    { model | url = url }


setDragState : DragState -> Model -> Model
setDragState dragState model =
    { model | dragState = dragState }



-- UPDATE


update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
    case msg of
        UrlSet newUrl ->
            let
                newModel =
                    model
                        |> setUrl newUrl
            in
            ( ( newModel, Cmd.none ), NoOp )

        EditImage ->
            ( ( model, Cmd.none ), ImageSelected model.url )

        DragEnter ->
            let
                newModel =
                    model
                        |> setDragState Hovering
            in
            ( ( newModel, Cmd.none ), NoOp )

        DragLeave ->
            let
                newModel =
                    model
                        |> setDragState Normal
            in
            ( ( newModel, Cmd.none ), NoOp )

        FileUpload fileInstances ->
            let
                newModel =
                    model
                        |> setDragState Normal

                cmd =
                    case List.head fileInstances of
                        Just fileInstance ->
                            loadData fileInstance.blob

                        Nothing ->
                            Cmd.none
            in
            ( ( newModel, cmd ), NoOp )

        FileData (Ok dataValue) ->
            let
                decodedResult =
                    decodeValue Decode.string dataValue

                externalMsg =
                    case decodedResult of
                        Ok str ->
                            ImageSelected str

                        option2 ->
                            NoOp
            in
            ( ( model, Cmd.none ), externalMsg )

        FileData (Err err) ->
            ( ( model, Cmd.none ), NoOp )



-- TASKS


loadData : FileRef -> Cmd Msg
loadData file =
    FileReader.readAsDataUrl file
        |> Task.map Ok
        |> Task.onError (Task.succeed << Err)
        |> Task.perform FileData



-- VIEW


view : Model -> Element Styles Styles.Variations Msg
view model =
    label Styles.HomeDropzone
        [ width (fill 1.0)
        , height (fill 1.0)
        , onDragEnter DragEnter
        , onDragLeave DragLeave
        , onDragOver DragEnter
        , onDrop FileUpload
        , vary Styles.HomeDropzoneHovering (model.dragState == Hovering)
        ]
        viewContent
    <|
        viewFileInput


viewContent : Element Styles variation Msg
viewContent =
    Element.column Styles.None
        [ center
        , verticalCenter
        , height (fill 1.0)
        , spacing 40
        , onDrop FileUpload
        ]
        [ Element.el Styles.PageHeader [] (Element.text "Emojify")
        , Element.el Styles.HomeDropzoneText [] (Element.text "Drop an image to begin")
        ]


viewFileInput : Element Styles variation Msg
viewFileInput =
    node "input" <|
        el Styles.None
            [ hidden
            , type_ "file"
            , multiple False
            , toAttr (FileReader.onFileChange FileUpload)
            ]
            empty



-- Some view handler functions


onDragEnter : a -> Attribute variation a
onDragEnter =
    onDragFunctionIgnoreFiles "dragenter"


onDragOver : a -> Attribute variation a
onDragOver =
    onDragFunctionIgnoreFiles "dragover"


onDragLeave : a -> Attribute variation a
onDragLeave =
    onDragFunctionIgnoreFiles "dragleave"


onDrop : (List NativeFile -> a) -> Attribute variation a
onDrop =
    onDragFunctionDecodeFiles "drop"


onDragFunctionIgnoreFiles :
    String
    -> a
    -> Attribute variation a
onDragFunctionIgnoreFiles nativeEventName action =
    Events.onWithOptions
        nativeEventName
        { stopPropagation = False, preventDefault = True }
        (Decode.map (\_ -> action) Decode.value)


onDragFunctionDecodeFiles :
    String
    -> (List NativeFile -> value)
    -> Attribute variation value
onDragFunctionDecodeFiles nativeEventName actionCreator =
    Events.onWithOptions
        nativeEventName
        { stopPropagation = True, preventDefault = True }
        (Decode.map actionCreator parseDroppedFiles)
