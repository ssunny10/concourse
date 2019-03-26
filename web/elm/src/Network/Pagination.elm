module Network.Pagination exposing (fetch, parseLinks)

import Concourse.Pagination exposing (Direction(..), Page, Paginated, Pagination)
import Dict exposing (Dict)
import Http
import Json.Decode
import Maybe.Extra
import Regex exposing (Regex)
import String
import Task exposing (Task)


fetch : Json.Decode.Decoder a -> String -> Maybe Page -> Task Http.Error (Paginated a)
fetch decode url page =
    Http.toTask <|
        Http.request
            { method = "GET"
            , headers = []
            , url = addParams url page
            , body = Http.emptyBody
            , expect = Http.expectStringResponse (parsePagination decode)
            , timeout = Nothing
            , withCredentials = False
            }


addParams : String -> Maybe Page -> String
addParams url page =
    let
        ( baseURL, query ) =
            extractQuery url
    in
    setQuery baseURL (Dict.union query (toQuery page))


parseParams : String -> Maybe Page
parseParams =
    fromQuery << Tuple.second << extractQuery


extractQuery : String -> ( String, Dict String String )
extractQuery url =
    case String.split "?" url of
        baseURL :: query :: _ ->
            ( baseURL, parseQuery query )

        _ ->
            ( url, Dict.empty )


setQuery : String -> Dict String String -> String
setQuery baseURL query =
    let
        params =
            String.join "&" <|
                List.map (\( k, v ) -> k ++ "=" ++ v) (Dict.toList query)
    in
    if params == "" then
        baseURL

    else
        baseURL ++ "?" ++ params


parsePagination : Json.Decode.Decoder a -> Http.Response String -> Result String (Paginated a)
parsePagination decode response =
    let
        pagination =
            parseLinks response

        decoded =
            Json.Decode.decodeString (Json.Decode.list decode) response.body
    in
    case decoded of
        Err err ->
            Err <| Debug.toString err

        Ok content ->
            Ok { content = content, pagination = pagination }


parseLinks : Http.Response String -> Pagination
parseLinks response =
    case Dict.get "link" <| keysToLower response.headers of
        Nothing ->
            Pagination Nothing Nothing

        Just commaSeparatedCraziness ->
            let
                headers =
                    String.split ", " commaSeparatedCraziness

                parsed =
                    Dict.fromList <| List.filterMap parseLinkTuple headers
            in
            Pagination
                (Dict.get previousRel parsed |> Maybe.andThen parseParams)
                (Dict.get nextRel parsed |> Maybe.andThen parseParams)


fromQuery : Dict String String -> Maybe Page
fromQuery query =
    let
        limit =
            Maybe.withDefault 0 <|
                (Dict.get "limit" query |> Maybe.andThen String.toInt)

        until =
            Maybe.map Until <|
                (Dict.get "until" query |> Maybe.andThen String.toInt)

        since =
            Maybe.map Since <|
                (Dict.get "since" query |> Maybe.andThen String.toInt)

        from =
            Maybe.map Since <|
                (Dict.get "from" query |> Maybe.andThen String.toInt)

        to =
            Maybe.map Since <|
                (Dict.get "to" query |> Maybe.andThen String.toInt)
    in
    Maybe.map (\direction -> { direction = direction, limit = limit }) <|
        Maybe.Extra.or until <|
            Maybe.Extra.or since <|
                Maybe.Extra.or from to


toQuery : Maybe Page -> Dict String String
toQuery page =
    case page of
        Nothing ->
            Dict.empty

        Just somePage ->
            let
                directionParam =
                    case somePage.direction of
                        Since id ->
                            ( "since", String.fromInt id )

                        Until id ->
                            ( "until", String.fromInt id )

                        From id ->
                            ( "from", String.fromInt id )

                        To id ->
                            ( "to", String.fromInt id )

                limitParam =
                    ( "limit", String.fromInt somePage.limit )
            in
            Dict.fromList [ directionParam, limitParam ]


parseLinkTuple : String -> Maybe ( String, String )
parseLinkTuple header =
    case
        Maybe.map (\r -> Regex.findAtMost 1 r header) linkHeaderRegex
            |> Maybe.withDefault []
    of
        [] ->
            Nothing

        { submatches } :: _ ->
            case submatches of
                (Just url) :: (Just rel) :: _ ->
                    Just ( rel, url )

                _ ->
                    Nothing


parseQuery : String -> Dict String String
parseQuery query =
    let
        parseParam p =
            case String.split "=" p of
                k :: vs ->
                    ( k, String.join "=" vs )

                [] ->
                    ( "", "" )
    in
    Dict.fromList <|
        List.map parseParam <|
            String.split "&" query


keysToLower : Dict String a -> Dict String a
keysToLower =
    Dict.toList
        >> List.map (Tuple.mapFirst String.toLower)
        >> Dict.fromList


previousRel : String
previousRel =
    "previous"


nextRel : String
nextRel =
    "next"


linkHeaderRegex : Maybe Regex
linkHeaderRegex =
    Regex.fromString ("<([^>]+)>; rel=\"(" ++ previousRel ++ "|" ++ nextRel ++ ")\"")
