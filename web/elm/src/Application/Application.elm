module Application.Application exposing
    ( Flags
    , Model
    , handleCallback
    , init
    , locationMsg
    , subscriptions
    , update
    , view
    )

import Concourse
import Html exposing (Html)
import Http
import Message.Callback exposing (Callback(..))
import Message.Effects as Effects exposing (Effect(..))
import Message.Subscription exposing (Delivery(..), Interval(..), Subscription(..))
import Message.TopLevelMessage as Msgs exposing (TopLevelMessage(..))
import Routes
import SubPage.SubPage as SubPage
import UserState exposing (UserState(..))


type alias Flags =
    { turbulenceImgSrc : String
    , notFoundImgSrc : String
    , csrfToken : Concourse.CSRFToken
    , authToken : String
    , pipelineRunningKeyframes : String
    }


type alias Model =
    { subModel : SubPage.Model
    , turbulenceImgSrc : String
    , notFoundImgSrc : String
    , csrfToken : String
    , authToken : String
    , pipelineRunningKeyframes : String
    , route : Routes.Route
    , userState : UserState
    }


init : Flags -> Url.Url -> Navigation.Key -> ( Model, List Effect )
init flags location =
    let
        route =
            Routes.parsePath location

        ( subModel, subEffects ) =
            SubPage.init
                { turbulencePath = flags.turbulenceImgSrc
                , authToken = flags.authToken
                , pipelineRunningKeyframes = flags.pipelineRunningKeyframes
                }
                route

        model =
            { subModel = subModel
            , turbulenceImgSrc = flags.turbulenceImgSrc
            , notFoundImgSrc = flags.notFoundImgSrc
            , csrfToken = flags.csrfToken
            , authToken = flags.authToken
            , pipelineRunningKeyframes = flags.pipelineRunningKeyframes
            , route = route
            , userState = UserStateUnknown
            }

        handleTokenEffect =
            -- We've refreshed on the page and we're not
            -- getting it from query params
            if flags.csrfToken == "" then
                [ LoadToken ]

            else
                [ SaveToken flags.csrfToken
                , Effects.ModifyUrl <| Routes.toString route
                ]
    in
    ( model, [ FetchUser ] ++ handleTokenEffect ++ subEffects )


locationMsg : Url.Url -> TopLevelMessage
locationMsg =
    Routes.parsePath >> RouteChanged >> DeliveryReceived


handleCallback : Callback -> Model -> ( Model, List Effect )
handleCallback callback model =
    case callback of
        BuildTriggered (Err err) ->
            ( model, redirectToLoginIfNecessary model err )

        BuildAborted (Err err) ->
            ( model, redirectToLoginIfNecessary model err )

        PausedToggled (Err err) ->
            ( model, redirectToLoginIfNecessary model err )

        JobBuildsFetched (Err err) ->
            ( model, redirectToLoginIfNecessary model err )

        InputToFetched (Err err) ->
            ( model, redirectToLoginIfNecessary model err )

        OutputOfFetched (Err err) ->
            ( model, redirectToLoginIfNecessary model err )

        LoggedOut (Ok ()) ->
            subpageHandleCallback { model | userState = UserStateLoggedOut } callback

        APIDataFetched (Ok ( time, data )) ->
            subpageHandleCallback
                { model | userState = data.user |> Maybe.map UserStateLoggedIn |> Maybe.withDefault UserStateLoggedOut }
                callback

        APIDataFetched (Err err) ->
            subpageHandleCallback { model | userState = UserStateLoggedOut } callback

        UserFetched (Ok user) ->
            subpageHandleCallback { model | userState = UserStateLoggedIn user } callback

        UserFetched (Err _) ->
            subpageHandleCallback { model | userState = UserStateLoggedOut } callback

        -- otherwise, pass down
        _ ->
            subpageHandleCallback model callback


subpageHandleCallback : Model -> Callback -> ( Model, List Effect )
subpageHandleCallback model callback =
    let
        ( subModel, effects ) =
            ( model.subModel, [] )
                |> SubPage.handleCallback callback
                |> SubPage.handleNotFound model.notFoundImgSrc model.route
    in
    ( { model | subModel = subModel }, effects )


update : TopLevelMessage -> Model -> ( Model, List Effect )
update msg model =
    case msg of
        Update m ->
            let
                ( subModel, subEffects ) =
                    ( model.subModel, [] )
                        |> SubPage.update m
                        |> SubPage.handleNotFound model.notFoundImgSrc model.route
            in
            ( { model | subModel = subModel }, subEffects )

        Callback callback ->
            handleCallback callback model

        DeliveryReceived delivery ->
            handleDelivery delivery model


handleDelivery : Delivery -> Model -> ( Model, List Effect )
handleDelivery delivery model =
    let
        ( newSubmodel, subPageEffects ) =
            ( model.subModel, [] )
                |> SubPage.handleDelivery delivery
                |> SubPage.handleNotFound model.notFoundImgSrc model.route

        ( newModel, applicationEffects ) =
            handleDeliveryForApplication
                delivery
                { model | subModel = newSubmodel }
    in
    ( newModel, subPageEffects ++ applicationEffects )


handleDeliveryForApplication : Delivery -> Model -> ( Model, List Effect )
handleDeliveryForApplication delivery model =
    case delivery of
        NonHrefLinkClicked route ->
            ( model, [ NavigateTo route ] )

        TokenReceived (Just tokenValue) ->
            ( { model | csrfToken = tokenValue }, [] )

        RouteChanged route ->
            urlUpdate route model

        _ ->
            ( model, [] )


redirectToLoginIfNecessary : Model -> Http.Error -> List Effect
redirectToLoginIfNecessary model err =
    case err of
        Http.BadStatus { status } ->
            if status.code == 401 then
                [ RedirectToLogin ]

            else
                []

        _ ->
            []


urlUpdate : Routes.Route -> Model -> ( Model, List Effect )
urlUpdate route model =
    let
        ( newSubmodel, subEffects ) =
            if route == model.route then
                ( model.subModel, [] )

            else if routeMatchesModel route model then
                SubPage.urlUpdate route ( model.subModel, [] )

            else
                SubPage.init
                    { turbulencePath = model.turbulenceImgSrc
                    , authToken = model.authToken
                    , pipelineRunningKeyframes = model.pipelineRunningKeyframes
                    }
                    route
    in
    ( { model | subModel = newSubmodel, route = route }
    , subEffects ++ [ SetFavIcon Nothing ]
    )


view : Model -> Html TopLevelMessage
view model =
    Html.map Update (SubPage.view model.userState model.subModel)


subscriptions : Model -> List Subscription
subscriptions model =
    [ OnNonHrefLinkClicked
    , OnTokenReceived
    ]
        ++ SubPage.subscriptions model.subModel


routeMatchesModel : Routes.Route -> Model -> Bool
routeMatchesModel route model =
    case ( route, model.subModel ) of
        ( Routes.Pipeline _, SubPage.PipelineModel _ ) ->
            True

        ( Routes.Resource _, SubPage.ResourceModel _ ) ->
            True

        ( Routes.Build _, SubPage.BuildModel _ ) ->
            True

        ( Routes.Job _, SubPage.JobModel _ ) ->
            True

        ( Routes.Dashboard _, SubPage.DashboardModel _ ) ->
            True

        _ ->
            False
