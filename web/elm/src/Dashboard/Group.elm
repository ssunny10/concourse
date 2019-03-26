module Dashboard.Group exposing
    ( PipelineIndex
    , allPipelines
    , allTeamNames
    , dragIndex
    , dragIndexOptional
    , dropIndex
    , dropIndexOptional
    , findGroupOptional
    , group
    , groups
    , hdView
    , jobStatus
    , ordering
    , pipelineDropAreaView
    , pipelineNotSetView
    , pipelineStatus
    , setDragIndex
    , setDropIndex
    , setTeamName
    , shiftPipelineTo
    , shiftPipelines
    , teamName
    , teamNameOptional
    , transition
    , view
    )

import Concourse
import Concourse.BuildStatus
import Concourse.PipelineStatus as PipelineStatus
import Dashboard.Group.Models exposing (Group, Pipeline)
import Dashboard.Group.Tag as Tag
import Dashboard.Models as Models exposing (DragState(..), DropState(..))
import Dashboard.Pipeline as Pipeline
import Dashboard.Styles as Styles
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onMouseEnter)
import Json.Decode
import List.Extra
import Maybe.Extra
import Message.Effects as Effects
import Message.Message exposing (Hoverable(..), Message(..))
import Monocle.Optional
import Ordering exposing (Ordering)
import Set
import Time exposing (Time)
import UserState exposing (UserState)


ordering : Ordering Group
ordering =
    Ordering.byFieldWith Tag.ordering .tag
        |> Ordering.breakTiesWith (Ordering.byField .teamName)


findGroupOptional : String -> Monocle.Optional.Optional (List Group) Group
findGroupOptional teamName =
    let
        predicate =
            .teamName >> (==) teamName
    in
    Monocle.Optional.Optional (List.Extra.find predicate)
        (\g gs ->
            List.Extra.findIndex predicate gs
                |> Maybe.andThen (\i -> List.Extra.setAt i g gs)
                |> Maybe.withDefault gs
        )


type alias PipelineIndex =
    Int


teamNameOptional : Monocle.Optional.Optional DragState Concourse.TeamName
teamNameOptional =
    Monocle.Optional.Optional teamName setTeamName


dragIndexOptional : Monocle.Optional.Optional DragState PipelineIndex
dragIndexOptional =
    Monocle.Optional.Optional dragIndex setDragIndex


dropIndexOptional : Monocle.Optional.Optional DropState PipelineIndex
dropIndexOptional =
    Monocle.Optional.Optional dropIndex setDropIndex


teamName : DragState -> Maybe Concourse.TeamName
teamName dragState =
    case dragState of
        Dragging teamName _ ->
            Just teamName

        NotDragging ->
            Nothing


setTeamName : Concourse.TeamName -> DragState -> DragState
setTeamName teamName dragState =
    case dragState of
        Dragging _ dragIndex ->
            Dragging teamName dragIndex

        NotDragging ->
            NotDragging


dragIndex : DragState -> Maybe PipelineIndex
dragIndex dragState =
    case dragState of
        Dragging _ dragIndex ->
            Just dragIndex

        NotDragging ->
            Nothing


setDragIndex : PipelineIndex -> DragState -> DragState
setDragIndex dragIndex dragState =
    case dragState of
        Dragging teamName _ ->
            Dragging teamName dragIndex

        NotDragging ->
            NotDragging


dropIndex : DropState -> Maybe PipelineIndex
dropIndex dropState =
    case dropState of
        Dropping dropIndex ->
            Just dropIndex

        NotDropping ->
            Nothing


setDropIndex : PipelineIndex -> DropState -> DropState
setDropIndex dropIndex dropState =
    case dropState of
        Dropping _ ->
            Dropping dropIndex

        NotDropping ->
            NotDropping


allPipelines : Concourse.APIData -> List Pipeline
allPipelines data =
    data.pipelines
        |> List.map
            (\p ->
                let
                    jobs =
                        data.jobs
                            |> List.filter
                                (\j ->
                                    (j.teamName == p.teamName)
                                        && (j.pipelineName == p.name)
                                )
                in
                { id = p.id
                , name = p.name
                , teamName = p.teamName
                , public = p.public
                , jobs = jobs
                , resourceError =
                    data.resources
                        |> List.any
                            (\r ->
                                (r.teamName == p.teamName)
                                    && (r.pipelineName == p.name)
                                    && r.failingToCheck
                            )
                , status = pipelineStatus p jobs
                , isToggleLoading = False
                }
            )


pipelineStatus : Concourse.Pipeline -> List Concourse.Job -> PipelineStatus.PipelineStatus
pipelineStatus pipeline jobs =
    if pipeline.paused then
        PipelineStatus.PipelineStatusPaused

    else
        let
            isRunning =
                List.any (\job -> job.nextBuild /= Nothing) jobs

            mostImportantJobStatus =
                jobs
                    |> List.map jobStatus
                    |> List.sortWith Concourse.BuildStatus.ordering
                    |> List.head

            firstNonSuccess =
                jobs
                    |> List.filter (jobStatus >> (/=) Concourse.BuildStatusSucceeded)
                    |> List.filterMap transition
                    |> List.sort
                    |> List.head

            lastTransition =
                jobs
                    |> List.filterMap transition
                    |> List.sort
                    |> List.reverse
                    |> List.head

            transitionTime =
                case firstNonSuccess of
                    Just t ->
                        Just t

                    Nothing ->
                        lastTransition
        in
        case ( mostImportantJobStatus, transitionTime ) of
            ( _, Nothing ) ->
                PipelineStatus.PipelineStatusPending isRunning

            ( Nothing, _ ) ->
                PipelineStatus.PipelineStatusPending isRunning

            ( Just Concourse.BuildStatusPending, _ ) ->
                PipelineStatus.PipelineStatusPending isRunning

            ( Just Concourse.BuildStatusStarted, _ ) ->
                PipelineStatus.PipelineStatusPending isRunning

            ( Just Concourse.BuildStatusSucceeded, Just since ) ->
                if isRunning then
                    PipelineStatus.PipelineStatusSucceeded PipelineStatus.Running

                else
                    PipelineStatus.PipelineStatusSucceeded (PipelineStatus.Since since)

            ( Just Concourse.BuildStatusFailed, Just since ) ->
                if isRunning then
                    PipelineStatus.PipelineStatusFailed PipelineStatus.Running

                else
                    PipelineStatus.PipelineStatusFailed (PipelineStatus.Since since)

            ( Just Concourse.BuildStatusErrored, Just since ) ->
                if isRunning then
                    PipelineStatus.PipelineStatusErrored PipelineStatus.Running

                else
                    PipelineStatus.PipelineStatusErrored (PipelineStatus.Since since)

            ( Just Concourse.BuildStatusAborted, Just since ) ->
                if isRunning then
                    PipelineStatus.PipelineStatusAborted PipelineStatus.Running

                else
                    PipelineStatus.PipelineStatusAborted (PipelineStatus.Since since)


jobStatus : Concourse.Job -> Concourse.BuildStatus
jobStatus job =
    case job.finishedBuild of
        Just build ->
            build.status

        Nothing ->
            Concourse.BuildStatusPending


transition : Concourse.Job -> Maybe Time.Posix
transition =
    .transitionBuild >> Maybe.map (.duration >> .finishedAt)


shiftPipelines : Int -> Int -> Group -> Group
shiftPipelines dragIndex dropIndex group =
    if dragIndex == dropIndex then
        group

    else
        let
            pipelines =
                case
                    List.head <|
                        List.drop dragIndex <|
                            group.pipelines
                of
                    Nothing ->
                        group.pipelines

                    Just pipeline ->
                        shiftPipelineTo pipeline dropIndex group.pipelines
        in
        { group | pipelines = pipelines }



-- TODO this is pretty hard to reason about. really deeply nested and nasty. doesn't exactly relate
-- to the hd refactor as hd doesn't have the drag-and-drop feature, but it's a big contributor
-- to the 'length of this file' tire fire


shiftPipelineTo : Pipeline -> Int -> List Pipeline -> List Pipeline
shiftPipelineTo pipeline position pipelines =
    case pipelines of
        [] ->
            if position < 0 then
                []

            else
                [ pipeline ]

        p :: ps ->
            if p.teamName /= pipeline.teamName then
                p :: shiftPipelineTo pipeline position ps

            else if p == pipeline then
                shiftPipelineTo pipeline (position - 1) ps

            else if position == 0 then
                pipeline :: p :: shiftPipelineTo pipeline (position - 1) ps

            else
                p :: shiftPipelineTo pipeline (position - 1) ps


allTeamNames : Concourse.APIData -> List String
allTeamNames apiData =
    Set.union
        (Set.fromList (List.map .teamName apiData.pipelines))
        (Set.fromList (List.map .name apiData.teams))
        |> Set.toList


groups : Concourse.APIData -> List Group
groups apiData =
    let
        teamNames =
            allTeamNames apiData
    in
    teamNames
        |> List.map (group (allPipelines apiData) apiData.user)


group : List Pipeline -> Maybe Concourse.User -> String -> Group
group allPipelines user teamName =
    { pipelines = List.filter (.teamName >> (==) teamName) allPipelines
    , teamName = teamName
    , tag =
        case user of
            Just u ->
                Tag.tag u teamName

            Nothing ->
                Nothing
    }


view :
    { dragState : DragState
    , dropState : DropState
    , now : Time
    , hovered : Maybe Hoverable
    , pipelineRunningKeyframes : String
    , userState : UserState
    }
    -> Group
    -> Html Message
view { dragState, dropState, now, hovered, pipelineRunningKeyframes, userState } group =
    let
        pipelines =
            if List.isEmpty group.pipelines then
                [ Pipeline.pipelineNotSetView ]

            else
                List.append
                    (List.indexedMap
                        (\i pipeline ->
                            let
                                pipelineId =
                                    { pipelineName = pipeline.name
                                    , teamName = pipeline.teamName
                                    }
                            in
                            Html.div [ class "pipeline-wrapper" ]
                                [ pipelineDropAreaView dragState dropState group.teamName i
                                , Html.div
                                    [ classList
                                        [ ( "card", True )
                                        , ( "dragging"
                                          , dragState == Dragging pipeline.teamName i
                                          )
                                        ]
                                    , attribute "data-pipeline-name" pipeline.name
                                    , attribute
                                        "ondragstart"
                                        "event.dataTransfer.setData('text/plain', '');"
                                    , draggable "true"
                                    , on "dragstart"
                                        (Json.Decode.succeed (DragStart pipeline.teamName i))
                                    , on "dragend" (Json.Decode.succeed DragEnd)
                                    ]
                                    [ Pipeline.pipelineView
                                        { now = now
                                        , pipeline = pipeline
                                        , hovered =
                                            hovered
                                                == (Just <| PipelineButton pipelineId)
                                        , pipelineRunningKeyframes = pipelineRunningKeyframes
                                        , userState = userState
                                        }
                                    ]
                                ]
                        )
                        group.pipelines
                    )
                    [ pipelineDropAreaView dragState dropState group.teamName (List.length group.pipelines) ]
    in
    Html.div
        [ id group.teamName
        , class "dashboard-team-group"
        , attribute "data-team-name" group.teamName
        ]
        [ Html.div
            [ style "display" "flex"
            , style "align-items" "center"
            , class <| .sectionHeaderClass Effects.stickyHeaderConfig
            ]
            ([ Html.div
                [ class "dashboard-team-name" ]
                [ Html.text group.teamName ]
             ]
                ++ (Maybe.Extra.maybeToList <|
                        Maybe.map (Tag.view False) group.tag
                   )
            )
        , Html.div
            [ class <| .sectionBodyClass Effects.stickyHeaderConfig ]
            pipelines
        ]


hdView : String -> Group -> List (Html Message)
hdView pipelineRunningKeyframes group =
    let
        header =
            [ Html.div
                [ class "dashboard-team-name" ]
                [ Html.text group.teamName ]
            ]
                ++ (Maybe.Extra.maybeToList <| Maybe.map (Tag.view True) group.tag)

        teamPipelines =
            if List.isEmpty group.pipelines then
                [ pipelineNotSetView ]

            else
                group.pipelines
                    |> List.map
                        (\p ->
                            Pipeline.hdPipelineView
                                { pipeline = p
                                , pipelineRunningKeyframes = pipelineRunningKeyframes
                                }
                        )
    in
    case teamPipelines of
        [] ->
            header

        p :: ps ->
            -- Wrap the team name and the first pipeline together so
            -- the team name is not the last element in a column
            Html.div
                [ class "dashboard-team-name-wrapper"
                , style Styles.teamNameHd
                ]
                (header ++ [ p ])
                :: ps


pipelineNotSetView : Html Message
pipelineNotSetView =
    Html.div
        [ class "card" ]
        [ Html.div
            [ style Styles.noPipelineCardHd ]
            [ Html.div
                [ style Styles.noPipelineCardTextHd ]
                [ Html.text "no pipelines set" ]
            ]
        ]


pipelineDropAreaView : DragState -> DropState -> String -> Int -> Html Message
pipelineDropAreaView dragState dropState teamName index =
    let
        ( active, over ) =
            case ( dragState, dropState ) of
                ( Dragging team dragIndex, NotDropping ) ->
                    ( team == teamName, index == dragIndex )

                ( Dragging team dragIndex, Dropping dropIndex ) ->
                    ( team == teamName, index == dropIndex )

                _ ->
                    ( False, False )
    in
    Html.div
        [ classList [ ( "drop-area", True ), ( "active", active ), ( "over", over ), ( "animation", dropState /= NotDropping ) ]
        , on "dragenter" (Json.Decode.succeed (DragOver teamName index))
        ]
        [ Html.text "" ]
