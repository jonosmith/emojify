module Main exposing (main)

{-| Main app entry point - wires up the models, updates and views in the sub modules
-}

import Element exposing (Element)
import Html exposing (Html)
import Navigation exposing (Location)
import Pages.Editor as Editor
import Pages.Home as Home
import Route
import Styles exposing (Styles, Variations, stylesheet)


-- MODEL


type alias Model =
    { data : DataModel
    , page : PageState
    }


type PageState
    = Home Home.Model
    | Editor Editor.Model


type alias DataModel =
    { location : Maybe Location
    , url : String
    }



-- Setters


setLocation : Maybe Location -> DataModel -> DataModel
setLocation location dataModel =
    { dataModel | location = location }


setUrl : String -> DataModel -> DataModel
setUrl url dataModel =
    { dataModel | url = url }


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


setPage : PageState -> Model -> Model
setPage page model =
    { model | page = page }



-- INIT


init : Location -> ( Model, Cmd Msg )
init location =
    let
        model =
            { data =
                { location = Just location
                , url = ""
                }
            , page = Home Home.init
            }

        cmd =
            Navigation.modifyUrl (Route.routeToString Route.Home)
    in
    ( model, cmd )



-- UPDATE


type Msg
    = NoOp
    | SetRoute Location
    | PageHomeMsg Home.Msg
    | PageEditorMsg Editor.Msg
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
            goToNewPage location newModel

        PageHomeMsg sceneMsg ->
            case model.page of
                Home homeModel ->
                    let
                        ( ( newPageModel, newSceneCmd ), newSceneMsg ) =
                            Home.update sceneMsg homeModel

                        ( newModel, newCmd ) =
                            model
                                |> setPage (Home newPageModel)
                                |> updateHomeSceneExternalMsg newSceneMsg

                        newCommand =
                            Cmd.batch
                                [ Cmd.map PageHomeMsg newSceneCmd
                                , newCmd
                                ]
                    in
                    ( newModel, newCommand )

                _ ->
                    ( model, Cmd.none )



        PageEditorMsg sceneMsg ->
            case model.page of
                Editor editorModel ->
                    let
                        ( newEditorModel, newSceneMsg ) =
                            Editor.update sceneMsg editorModel

                        newModel =
                            model
                                |> setPage (Editor newEditorModel)
                    in
                    ( newModel, newSceneMsg )

                _ ->
                    ( model, Cmd.none )

        EditImage ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


updateHomeSceneExternalMsg : Home.ExternalMsg -> Model -> ( Model, Cmd Msg )
updateHomeSceneExternalMsg msg model =
    case msg of
        Home.ImageUploaded imageUrl ->
            let
                newModel =
                    model
                        |> setDataUrl imageUrl

                newCmd =
                    Navigation.newUrl (Route.routeToString Route.Editor)
            in
            ( newModel, newCmd )

        Home.NoOp ->
            ( model, Cmd.none )


goToNewPage : Location -> Model -> ( Model, Cmd Msg )
goToNewPage location model =
    case Route.parseLocation location of
        Route.Editor ->
            let
                ( editorInitModel, editorInitCmd ) =
                    Editor.init (Just model.data.url)

                newModel =
                    model
                        |> setPage (Editor editorInitModel)

                newCmd =
                    Cmd.map PageEditorMsg editorInitCmd
            in
            ( newModel, newCmd )

        _ ->
            let
                homeInitModel =
                    Home.init

                newModel =
                    model
                        |> setPage (Home homeInitModel)
            in
            ( newModel, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        sceneSubscriptions =
            case model.page of
                Home _ ->
                    Sub.none

                Editor editorModel ->
                    Sub.map PageEditorMsg (Editor.subscriptions editorModel)
    in
    Sub.batch
        [ sceneSubscriptions
        ]



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
    Navigation.program SetRoute
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
