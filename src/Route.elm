module Route exposing (..)

import Navigation exposing (Location)
import UrlParser as Url exposing ((</>), Parser, oneOf, parsePath, s, string)


type Route
    = Home
    | Editor
    | NotFoundRoute


routes : Parser (Route -> a) a
routes =
    oneOf
        [ Url.map Home (s "")
        , Url.map Editor (s "editor")
        ]


routeToString : Route -> String
routeToString route =
    let
        pieces =
            case route of
                Home ->
                    []

                Editor ->
                    [ "editor" ]

                NotFoundRoute ->
                    []
    in
        "/" ++ (String.join "/" pieces)


parseLocation : Location -> Route
parseLocation location =
    case (parsePath routes location) of
        Just route ->
            route

        Nothing ->
            NotFoundRoute
