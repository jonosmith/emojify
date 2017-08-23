module Main exposing (main)

{-| Main app entry point - wires up the sub models, updates and views
-}

import Element
import Html exposing (..)
import Navigation exposing (Location)
import Route
import Scenes.Home as Home
import Scenes.Editor as Editor
import Styles exposing (stylesheet)


-- MODEL


type alias Model =
    { data : DataModel
    , scenes : ScenesModel
    }


type alias ScenesModel =
    { currentScene : Scenes
    , home : Home.Model
    , editor : Editor.Model
    }


type Scenes
    = Home
    | Editor


type alias DataModel =
    { location : Maybe Location
    , url : String
    , isEditing : Bool
    }


setIsEditing : Bool -> DataModel -> DataModel
setIsEditing isEditing dataModel =
    { dataModel | isEditing = isEditing }


setLocation : Maybe Location -> DataModel -> DataModel
setLocation location dataModel =
    { dataModel | location = location }


setUrl : String -> DataModel -> DataModel
setUrl url dataModel =
    { dataModel | url = url }


setDataIsEditing : Bool -> Model -> Model
setDataIsEditing isEditing model =
    model.data
        |> setIsEditing isEditing
        |> asDataIn model


setDataLocation : Location -> Model -> Model
setDataLocation newLocation model =
    let
        dataModel =
            model.data
    in
        { dataModel | location = Just newLocation }
            |> asDataIn model


setDataUrl : String -> Model -> Model
setDataUrl newUrl model =
    model.data
        |> setUrl newUrl
        |> asDataIn model


setData : DataModel -> Model -> Model
setData dataModel model =
    { model | data = dataModel }


asDataIn : Model -> DataModel -> Model
asDataIn =
    flip setData


setScenes : ScenesModel -> Model -> Model
setScenes scenesModel model =
    { model | scenes = scenesModel }


asScenesIn : Model -> ScenesModel -> Model
asScenesIn =
    flip setScenes


setCurrentScene : Scenes -> ScenesModel -> ScenesModel
setCurrentScene currentScene scenesModel =
    { scenesModel | currentScene = currentScene }


setEditor : Editor.Model -> ScenesModel -> ScenesModel
setEditor editor scenesModel =
    { scenesModel | editor = editor }


setHome : Home.Model -> ScenesModel -> ScenesModel
setHome home scenesModel =
    { scenesModel | home = home }


setScenesCurrentScene : Scenes -> Model -> Model
setScenesCurrentScene currentScene model =
    model.scenes
        |> setCurrentScene currentScene
        |> asScenesIn model


setScenesEditor : Editor.Model -> Model -> Model
setScenesEditor editorModel model =
    model.scenes
        |> setEditor editorModel
        |> asScenesIn model


setScenesHome : Home.Model -> Model -> Model
setScenesHome homeModel model =
    model.scenes
        |> setHome homeModel
        |> asScenesIn model


init : Location -> ( Model, Cmd Msg )
init location =
    let
        currentRoute =
            Route.parseLocation location

        currentScene =
            case currentRoute of
                Route.Home ->
                    Home

                Route.Editor ->
                    Editor

                Route.NotFoundRoute ->
                    Home

        ( editorInitModel, editorInitCmd ) =
            Editor.init Nothing

        model =
            { data =
                { location = Just location
                , url = ""
                , isEditing = False
                }
            , scenes =
                { currentScene = currentScene
                , home = Home.init
                , editor = editorInitModel
                }
            }

        command =
            Cmd.batch
                [ (Navigation.modifyUrl (Route.routeToString Route.Home))
                , Cmd.map SceneEditorMsg editorInitCmd
                ]
    in
        ( model, command )



-- UPDATE


type Msg
    = NoOp
    | SetRoute Location
    | SceneHomeMsg Home.Msg
    | SceneEditorMsg Editor.Msg
    | EditImage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetRoute location ->
            let
                newModel =
                    model
                        |> setDataLocation location
            in
                ( newModel, Cmd.none )

        SceneHomeMsg msg ->
            let
                ( ( newPageModel, pageCmd ), msgFromPage ) =
                    Home.update msg model.scenes.home

                ( newModel, newCmd ) =
                    model
                        |> setScenesHome newPageModel
                        |> updateHomeSceneExternalMsg msgFromPage

                newCommand =
                    Cmd.batch
                        [ (Cmd.map SceneHomeMsg pageCmd)
                        , newCmd
                        ]
            in
                ( newModel, newCommand )

        SceneEditorMsg msg ->
            let
                ( newEditorModel, sceneMsg ) =
                    Editor.update msg model.scenes.editor

                newModel =
                    model
                        |> setScenesEditor newEditorModel
            in
                ( newModel, sceneMsg )

        EditImage ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


updateHomeSceneExternalMsg : Home.ExternalMsg -> Model -> ( Model, Cmd Msg )
updateHomeSceneExternalMsg msg model =
    case msg of
        Home.ImageSelected imageUrl ->
            let
                ( editorInitModel, editorInitCmd ) =
                    Editor.init (Just imageUrl)

                newModel =
                    model
                        |> setDataUrl imageUrl
                        |> setDataIsEditing True
                        |> setScenesEditor editorInitModel

                newCmd =
                    Cmd.batch
                        [ (Navigation.newUrl (Route.routeToString Route.Editor))
                        , Cmd.map SceneEditorMsg editorInitCmd
                        ]
            in
                ( newModel, newCmd )

        Home.NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        sceneSubscriptions =
            case model.scenes.currentScene of
                Home ->
                    Sub.none

                Editor ->
                    Sub.map SceneEditorMsg (Editor.subscriptions model.scenes.editor)
    in
        Sub.batch
            [ sceneSubscriptions
            ]



-- VIEW


view : Model -> Html Msg
view model =
    let
        location =
            model.data.location

        route =
            case location of
                Just location ->
                    Route.parseLocation location

                Nothing ->
                    Route.NotFoundRoute
    in
        Element.root stylesheet <|
            case route of
                Route.Home ->
                    viewHome model

                Route.Editor ->
                    Element.map SceneEditorMsg (Editor.view model.data.url model.scenes.editor)

                Route.NotFoundRoute ->
                    viewHome model


viewHome model =
    Element.map SceneHomeMsg (Home.view model.scenes.home)



-- PROGRAM


main : Program Never Model Msg
main =
    Navigation.program SetRoute
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
