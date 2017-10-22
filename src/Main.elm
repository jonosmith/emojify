module Main exposing (main)

{-| Main app entry point - wires up the models, updates and views in the sub modules (pages)
-}

import Element exposing (Element)
import Html exposing (Html)
import Navigation exposing (Location)
import Pages.Editor as Editor
import Pages.Home as Home
import Route
import Styles exposing (Styles, Variations, stylesheet)
import Util exposing (toTupleWith)


-- MODEL


type alias Model =
    { app : AppModel
    , page : PageModel
    }


type PageModel
    = Home Home.Model
    | Editor Editor.Model


type alias AppModel =
    { location : Maybe Location
    , imageUrl : String
    }



-- Setters


setAppLocation : Location -> Model -> Model
setAppLocation location model =
    let
        appModel =
            model.app
    in
    { model | app = { appModel | location = Just location } }



-- INIT


init : Location -> ( Model, Cmd Msg )
init location =
    let
        model =
            { app =
                { location = Just location
                , imageUrl = ""
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
            goToNewLocation location model

        PageHomeMsg pageMsg ->
            case model.page of
                Home homeModel ->
                    let
                        ( ( newPageModel, newPageCmd ), msgFromPage ) =
                            Home.update pageMsg homeModel

                        ( newModel, newCmd ) =
                            case msgFromPage of
                                Home.ImageUploaded imageUrl ->
                                    let
                                        appModel =
                                            model.app
                                    in
                                    { model | app = { appModel | imageUrl = imageUrl } }
                                        |> toTupleWith (goToNewRoute Route.Editor)

                                Home.NoOp ->
                                    ( model, Cmd.none )

                        newCommand =
                            Cmd.batch
                                [ Cmd.map PageHomeMsg newPageCmd
                                , newCmd
                                ]
                    in
                    ( { newModel | page = Home newPageModel }, newCommand )

                _ ->
                    ( model, Cmd.none )

        PageEditorMsg pageMsg ->
            case model.page of
                Editor editorModel ->
                    let
                        ( newEditorModel, newPageMsg ) =
                            Editor.update pageMsg editorModel
                    in
                    ( { model | page = Editor newEditorModel }, newPageMsg )

                _ ->
                    ( model, Cmd.none )

        EditImage ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


goToNewRoute : Route.Route -> Cmd msg
goToNewRoute route =
    Navigation.newUrl (Route.routeToString route)


goToNewLocation : Location -> Model -> ( Model, Cmd Msg )
goToNewLocation location model =
    let
        newModel =
            setAppLocation location model
    in
    goToLocationPage location newModel


goToLocationPage : Location -> Model -> ( Model, Cmd Msg )
goToLocationPage location model =
    case Route.parseLocation location of
        Route.Editor ->
            let
                ( editorInitModel, editorInitCmd ) =
                    Editor.init (Just model.app.imageUrl)

                newCmd =
                    Cmd.map PageEditorMsg editorInitCmd
            in
            ( { model | page = Editor editorInitModel }, newCmd )

        _ ->
            ( { model | page = Home Home.init }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        pageSubscriptions =
            case model.page of
                Home _ ->
                    Sub.none

                Editor editorModel ->
                    Sub.map PageEditorMsg (Editor.subscriptions editorModel)
    in
    Sub.batch
        [ pageSubscriptions
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
