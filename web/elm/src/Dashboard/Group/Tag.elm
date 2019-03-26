module Dashboard.Group.Tag exposing (Tag(..), ordering, splitFirst, tag, view)

import Colors
import Concourse
import Dict
import Html exposing (Html)
import Html.Attributes exposing (class, style)
import List.Extra
import Ordering exposing (Ordering)


type Tag
    = Owner
    | Member
    | Viewer


ordering : Ordering (Maybe Tag)
ordering =
    Ordering.explicit
        [ Just Owner
        , Just Member
        , Just Viewer
        , Nothing
        ]


view : Bool -> Tag -> Html msg
view isHd t =
    Html.div
        ([ style "border" ("1px solid " ++ Colors.white)
         , style "font-size" "0.7em"
         , style "padding" "0.5em 0"
         , style "line-height" "0.9em"
         , style "width" "6em"
         , style "text-align" "center"
         , style "letter-spacing" "0.2em"
         ]
            ++ (if isHd then
                    [ style "margin-bottom" "1em" ]

                else
                    [ style "margin-bottom" "" ]
               )
        )
        [ Html.text <| toString t ]


toString : Tag -> String
toString t =
    case t of
        Owner ->
            "OWNER"

        Member ->
            "MEMBER"

        Viewer ->
            "VIEWER"


splitFirst : Char -> String -> String
splitFirst delim =
    String.toList
        >> List.Extra.takeWhile ((/=) delim)
        >> String.fromList


tag : Concourse.User -> String -> Maybe Tag
tag user teamName =
    case Dict.get teamName user.teams of
        Just roles ->
            firstRole roles

        Nothing ->
            Nothing


firstRole : List String -> Maybe Tag
firstRole roles =
    case List.head roles of
        Just rs ->
            parseRole rs

        Nothing ->
            Nothing


parseRole : String -> Maybe Tag
parseRole role =
    case role of
        "owner" ->
            Just Owner

        "member" ->
            Just Member

        "viewer" ->
            Just Viewer

        _ ->
            Nothing
