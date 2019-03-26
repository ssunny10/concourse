module Build.Output.Output exposing
    ( OutMsg(..)
    , handleEnvelopes
    , handleStepTreeMsg
    , init
    , planAndResourcesFetched
    , view
    )

import Ansi.Log
import Array exposing (Array)
import Build.Output.Models exposing (OutputModel, OutputState(..))
import Build.StepTree.Models as StepTree
    exposing
        ( BuildEvent(..)
        , BuildEventEnvelope
        , StepState(..)
        , StepTree
        , StepTreeModel
        )
import Build.StepTree.StepTree
import Build.Styles as Styles
import Concourse
import Concourse.BuildStatus
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
    exposing
        ( action
        , class
        , classList
        , id
        , method
        , style
        , title
        )
import Http
import Message.Effects exposing (Effect(..))
import Message.Message exposing (Message(..))
import Routes exposing (StepID)
import Views.Icon as Icon
import Views.LoadingIndicator as LoadingIndicator
import Views.NotAuthorized as NotAuthorized


type OutMsg
    = OutNoop
    | OutBuildStatus Concourse.BuildStatus Time.Posix


type alias Flags =
    { highlight : Routes.Highlight }


init : Flags -> Concourse.Build -> ( OutputModel, List Effect )
init { highlight } build =
    let
        outputState =
            if Concourse.BuildStatus.isRunning build.status then
                StepsLiveUpdating

            else
                StepsLoading

        model =
            { steps = Nothing
            , errors = Nothing
            , state = outputState
            , eventStreamUrlPath = Nothing
            , eventSourceOpened = False
            , highlight = highlight
            }

        fetch =
            if build.job /= Nothing then
                [ FetchBuildPlanAndResources build.id ]

            else
                [ FetchBuildPlan build.id ]
    in
    ( model, fetch )


handleStepTreeMsg :
    (StepTreeModel -> ( StepTreeModel, List Effect ))
    -> OutputModel
    -> ( OutputModel, List Effect, OutMsg )
handleStepTreeMsg action model =
    case model.steps of
        Just st ->
            let
                ( newModel, effects ) =
                    action st
            in
            ( { model | steps = Just newModel }, effects, OutNoop )

        _ ->
            ( model, [], OutNoop )


planAndResourcesFetched :
    Concourse.BuildId
    -> Result Http.Error ( Concourse.BuildPlan, Concourse.BuildResources )
    -> OutputModel
    -> ( OutputModel, List Effect, OutMsg )
planAndResourcesFetched buildId result model =
    let
        url =
            "/api/v1/builds/" ++ toString buildId ++ "/events"
    in
    ( case result of
        Err err ->
            case err of
                Http.BadStatus { status } ->
                    if status.code == 404 then
                        { model | eventStreamUrlPath = Just url }

                    else
                        model

                _ ->
                    (\a -> always a (Debug.log "failed to fetch plan" err)) <|
                        model

        Ok ( plan, resources ) ->
            { model
                | steps = Just (Build.StepTree.StepTree.init model.highlight resources plan)
                , eventStreamUrlPath = Just url
            }
    , []
    , OutNoop
    )


handleEnvelopes :
    Result String (List BuildEventEnvelope)
    -> OutputModel
    -> ( OutputModel, List Effect, OutMsg )
handleEnvelopes action model =
    case action of
        Ok envelopes ->
            envelopes
                |> List.reverse
                |> List.foldr handleEnvelope ( model, [], OutNoop )

        Err err ->
            (\a -> always a (Debug.log "failed to get event" err)) <|
                ( model, [], OutNoop )


handleEnvelope :
    BuildEventEnvelope
    -> ( OutputModel, List Effect, OutMsg )
    -> ( OutputModel, List Effect, OutMsg )
handleEnvelope { url, data } ( model, effects, outmsg ) =
    if
        model.eventStreamUrlPath
            |> Maybe.map (\p -> String.endsWith p url)
            |> Maybe.withDefault False
    then
        handleEvent data ( model, effects, outmsg )

    else
        ( model, effects, outmsg )


handleEvent :
    BuildEvent
    -> ( OutputModel, List Effect, OutMsg )
    -> ( OutputModel, List Effect, OutMsg )
handleEvent event ( model, effects, outmsg ) =
    case event of
        Opened ->
            ( { model | eventSourceOpened = True }
            , effects
            , outmsg
            )

        Log origin output time ->
            ( updateStep origin.id (setRunning << appendStepLog output time) model
            , effects
            , outmsg
            )

        Error origin message ->
            ( updateStep origin.id (setStepError message) model
            , effects
            , outmsg
            )

        Initialize origin ->
            ( updateStep origin.id setRunning model
            , effects
            , outmsg
            )

        StartTask origin ->
            ( updateStep origin.id setRunning model
            , effects
            , outmsg
            )

        FinishTask origin exitStatus ->
            ( updateStep origin.id (finishStep exitStatus) model
            , effects
            , outmsg
            )

        FinishGet origin exitStatus version metadata ->
            ( updateStep origin.id (finishStep exitStatus << setResourceInfo version metadata) model
            , effects
            , outmsg
            )

        FinishPut origin exitStatus version metadata ->
            ( updateStep origin.id (finishStep exitStatus << setResourceInfo version metadata) model
            , effects
            , outmsg
            )

        BuildStatus status date ->
            let
                newSt =
                    model.steps
                        |> Maybe.map
                            (\st ->
                                if Concourse.BuildStatus.isRunning status then
                                    st

                                else
                                    Build.StepTree.StepTree.finished st
                            )
            in
            ( { model | steps = newSt }, effects, OutBuildStatus status date )

        BuildError message ->
            ( { model
                | errors =
                    Just <|
                        Ansi.Log.update message <|
                            Maybe.withDefault (Ansi.Log.init Ansi.Log.Cooked) model.errors
              }
            , effects
            , outmsg
            )

        End ->
            ( { model | state = StepsComplete, eventStreamUrlPath = Nothing }
            , effects
            , outmsg
            )

        NetworkError ->
            if model.eventSourceOpened then
                -- connection could have dropped out of the blue;
                -- just let the browser handle reconnecting
                ( model, effects, outmsg )

            else
                -- assume request was rejected because auth is required;
                -- no way to really tell
                ( { model | state = NotAuthorized }, effects, outmsg )


updateStep : StepID -> (StepTree -> StepTree) -> OutputModel -> OutputModel
updateStep id update model =
    { model | steps = Maybe.map (StepTree.updateAt id update) model.steps }


setRunning : StepTree -> StepTree
setRunning =
    setStepState StepStateRunning


appendStepLog : String -> Maybe Time.Posix -> StepTree -> StepTree
appendStepLog output mtime tree =
    (\a -> StepTree.map a tree) <|
        \step ->
            let
                outputLineCount =
                    Ansi.Log.update output (Ansi.Log.init Ansi.Log.Cooked) |> .lines |> Array.length

                logLineCount =
                    max (Array.length step.log.lines - 1) 0

                setLineTimestamp line timestamps =
                    Dict.update line (\mval -> mtime) timestamps

                newTimestamps =
                    List.foldl
                        setLineTimestamp
                        step.timestamps
                        (List.range logLineCount (logLineCount + outputLineCount - 1))

                newLog =
                    Ansi.Log.update output step.log
            in
            { step | log = newLog, timestamps = newTimestamps }


setStepError : String -> StepTree -> StepTree
setStepError message tree =
    StepTree.map
        (\step ->
            { step
                | state = StepStateErrored
                , error = Just message
            }
        )
        tree


finishStep : Int -> StepTree -> StepTree
finishStep exitStatus tree =
    let
        stepState =
            if exitStatus == 0 then
                StepStateSucceeded

            else
                StepStateFailed
    in
    setStepState stepState tree


setResourceInfo : Concourse.Version -> Concourse.Metadata -> StepTree -> StepTree
setResourceInfo version metadata tree =
    StepTree.map (\step -> { step | version = Just version, metadata = metadata }) tree


setStepState : StepState -> StepTree -> StepTree
setStepState state tree =
    StepTree.map (\step -> { step | state = state }) tree


view : Concourse.Build -> OutputModel -> Html Message
view build { steps, errors, state } =
    Html.div [ class "steps" ]
        [ viewErrors errors
        , viewStepTree build steps state
        ]


viewStepTree :
    Concourse.Build
    -> Maybe StepTreeModel
    -> OutputState
    -> Html Message
viewStepTree build steps state =
    case ( state, steps ) of
        ( StepsLoading, _ ) ->
            LoadingIndicator.view

        ( NotAuthorized, _ ) ->
            NotAuthorized.view

        ( StepsLiveUpdating, Just root ) ->
            Build.StepTree.StepTree.view root

        ( StepsComplete, Just root ) ->
            Build.StepTree.StepTree.view root

        ( _, Nothing ) ->
            Html.div [] []


viewErrors : Maybe Ansi.Log.Model -> Html Message
viewErrors errors =
    case errors of
        Nothing ->
            Html.div [] []

        Just log ->
            Html.div [ class "build-step" ]
                [ Html.div [ class "header" ]
                    [ Icon.icon
                        { sizePx = 28
                        , image = "ic-exclamation-triangle.svg"
                        }
                        [ style Styles.stepStatusIcon ]
                    , Html.h3 [] [ Html.text "error" ]
                    ]
                , Html.div [ class "step-body build-errors-body" ]
                    [ Html.pre
                        []
                        (Array.toList (Array.map Ansi.Log.viewLine log.lines))
                    ]
                ]
