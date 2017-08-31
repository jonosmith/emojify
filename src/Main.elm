module Main exposing (main)

{-| Main app entry point - wires up the sub models, updates and views
-}

import Element exposing (Element)
import Html exposing (Html)
import Navigation exposing (Location)
import Route
import Scenes.Editor as Editor
import Scenes.Home as Home
import Styles exposing (Styles, Variations, stylesheet)


-- MODEL


type alias Model =
    { data : DataModel
    , scenes : ScenesModel
    }


type alias ScenesModel =
    { home : Home.Model
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



-- Setters


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
    dataModel
        |> setLocation (Just newLocation)
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


setEditor : Editor.Model -> ScenesModel -> ScenesModel
setEditor editor scenesModel =
    { scenesModel | editor = editor }


setHome : Home.Model -> ScenesModel -> ScenesModel
setHome home scenesModel =
    { scenesModel | home = home }


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



-- Derived


currentScene : Maybe Location -> Scenes
currentScene maybeLocation =
    let
        route =
            case maybeLocation of
                Just location ->
                    Route.parseLocation location

                Nothing ->
                    Route.NotFoundRoute
    in
    case route of
        Route.Home ->
            Home

        Route.Editor ->
            Editor

        Route.NotFoundRoute ->
            Home



-- Init model


init : Location -> ( Model, Cmd Msg )
init location =
    let
        ( editorInitModel, editorInitCmd ) =
            Editor.init Nothing

        model =
            { data =
                { location = Just location
                , url = ""
                , isEditing = False
                }
            , scenes =
                { home = Home.init
                , editor = editorInitModel
                }
            }

        command =
            Cmd.batch
                [ Navigation.modifyUrl (Route.routeToString Route.Home)
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

        SceneHomeMsg sceneMsg ->
            let
                ( ( newSceneModel, newSceneCmd ), newSceneMsg ) =
                    Home.update sceneMsg model.scenes.home

                ( newModel, newCmd ) =
                    model
                        |> setScenesHome newSceneModel
                        |> updateHomeSceneExternalMsg newSceneMsg

                newCommand =
                    Cmd.batch
                        [ Cmd.map SceneHomeMsg newSceneCmd
                        , newCmd
                        ]
            in
            ( newModel, newCommand )

        SceneEditorMsg sceneMsg ->
            let
                ( newEditorModel, newSceneMsg ) =
                    Editor.update sceneMsg model.scenes.editor

                newModel =
                    model
                        |> setScenesEditor newEditorModel
            in
            ( newModel, newSceneMsg )

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
                        [ Navigation.newUrl (Route.routeToString Route.Editor)
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
            case currentScene model.data.location of
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
    Element.root stylesheet <|
        case currentScene model.data.location of
            Home ->
                viewHome model

            Editor ->
                Element.map SceneEditorMsg (Editor.view model.scenes.editor)


viewHome : Model -> Element Styles Variations Msg
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
