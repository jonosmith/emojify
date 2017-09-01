module Scenes.Home
    exposing
        ( ExternalMsg(ImageUploaded, NoOp)
        , Model
        , Msg
        , init
        , update
        , view
        )

{-| Home scene - starting point for selecting an image to edit
-}

import Element exposing (Attribute, Element, el, empty, label, node)
import Element.Attributes exposing (..)
import Element.Events as Events
import FileReader exposing (Error, FileContentDataUrl, FileRef, NativeFile, parseDroppedFiles)
import Json.Decode as Decode exposing (decodeValue)
import Styles exposing (Styles)
import Task


type Msg
    = DragEnter
    | DragLeave
    | UploadFile (List NativeFile)
    | FileDataUrlReadComplete (Result Error FileContentDataUrl)


type ExternalMsg
    = NoOp
    | ImageUploaded String


type DragState
    = Normal
    | Hovering



-- MODEL


type alias Model =
    DragState


init : Model
init =
    Normal



-- UPDATE


update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
    case msg of
        DragEnter ->
            let
                newModel =
                    Hovering
            in
            ( ( newModel, Cmd.none ), NoOp )

        DragLeave ->
            let
                newModel =
                    Normal
            in
            ( ( newModel, Cmd.none ), NoOp )

        UploadFile fileInstances ->
            let
                newModel =
                    Normal

                cmd =
                    case List.head fileInstances of
                        Just fileInstance ->
                            loadData fileInstance.blob

                        Nothing ->
                            Cmd.none
            in
            ( ( newModel, cmd ), NoOp )

        FileDataUrlReadComplete (Ok dataValue) ->
            let
                decodedResult =
                    decodeValue Decode.string dataValue

                externalMsg =
                    case decodedResult of
                        Ok str ->
                            ImageUploaded str

                        Err _ ->
                            NoOp
            in
            ( ( model, Cmd.none ), externalMsg )

        FileDataUrlReadComplete (Err _) ->
            ( ( model, Cmd.none ), NoOp )



-- TASKS


loadData : FileRef -> Cmd Msg
loadData file =
    FileReader.readAsDataUrl file
        |> Task.map Ok
        |> Task.onError (Task.succeed << Err)
        |> Task.perform FileDataUrlReadComplete



-- VIEW


view : Model -> Element Styles Styles.Variations Msg
view model =
    label Styles.HomeDropzone
        [ width (fill 1.0)
        , height (fill 1.0)
        , onDragEnter DragEnter
        , onDragLeave DragLeave
        , onDragOver DragEnter
        , onDrop UploadFile
        , vary Styles.HomeDropzoneHovering (model == Hovering)
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
        , onDrop UploadFile
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
            , toAttr (FileReader.onFileChange UploadFile)
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
