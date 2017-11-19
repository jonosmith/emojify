module Main exposing (main)

{-| Main app entry point - wires up the models, updates and views in the sub modules (pages)
-}

import Element exposing (Element)
import Html exposing (Html, program)
import Pages.Editor as Editor
import Pages.Home as Home
import Styles exposing (Styles, Variations, stylesheet)


-- MODEL


type alias Model =
    { page : PageModel
    }


type PageModel
    = Home Home.Model
    | Editor Editor.Model



-- INIT


init : ( Model, Cmd Msg )
init =
    let
        model =
            { page = Home Home.init
            }
    in
    ( model, Cmd.none )



-- UPDATE


type Msg
    = NoOp
    | PageHomeMsg Home.Msg
    | PageEditorMsg Editor.Msg
    | EditImage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PageHomeMsg pageMsg ->
            case model.page of
                Home homeModel ->
                    let
                        ( ( newPageModel, newPageCmd ), msgFromPage ) =
                            Home.update pageMsg homeModel

                        ( newModel, newCmd ) =
                            case msgFromPage of
                                Home.ImageUploaded imageUrl ->
                                    handleNewImageUrl imageUrl { model | page = Home newPageModel }

                                Home.NoOp ->
                                    let
                                        newCommand =
                                            Cmd.map PageHomeMsg newPageCmd
                                    in
                                    ( { model | page = Home newPageModel }, newCommand )
                    in
                    ( newModel, newCmd )

                _ ->
                    ( model, Cmd.none )

        PageEditorMsg pageMsg ->
            case model.page of
                Editor editorModel ->
                    let
                        ( ( newPageModel, newPageCmd ), msgFromPage ) =
                            Editor.update pageMsg editorModel

                        ( newModel, newCmd ) =
                            case msgFromPage of
                                Editor.ResetApp ->
                                    init

                                Editor.NoOp ->
                                    let
                                        newCommand =
                                            Cmd.map PageEditorMsg newPageCmd
                                    in
                                    ( { model | page = Editor newPageModel }, newCommand )
                    in
                    ( newModel, newCmd )

                _ ->
                    ( model, Cmd.none )

        EditImage ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


handleNewImageUrl : String -> Model -> ( Model, Cmd Msg )
handleNewImageUrl imageUrl model =
    let
        ( editorModel, editorCmd ) =
            Editor.init (Just imageUrl)
    in
    ( { model | page = Editor editorModel }
    , Cmd.map PageEditorMsg editorCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        Home _ ->
            Sub.none

        Editor editorModel ->
            Sub.map PageEditorMsg (Editor.subscriptions editorModel)



-- VIEW


view : Model -> Html Msg
view model =
    Element.root stylesheet <|
        case model.page of
            Home homeModel ->
                viewHome homeModel

            Editor editorModel ->
                Element.map PageEditorMsg (Editor.view editorModel)


viewHome : Home.Model -> Element Styles Variations Msg
viewHome model =
    Element.map PageHomeMsg (Home.view model)



-- PROGRAM


main : Program Never Model Msg
main =
    program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
