port module Message.Subscription exposing
    ( Delivery(..)
    , Interval(..)
    , Subscription(..)
    , runSubscription
    )

import Browser.Events exposing (onClick, onKeyDown, onKeyUp, onMouseMove, onResize)
import Build.StepTree.Models exposing (BuildEventEnvelope)
import Concourse.BuildEvents exposing (decodeBuildEventEnvelope)
import Json.Decode
import Json.Encode
import Routes
import Time


port newUrl : (String -> msg) -> Sub msg


port tokenReceived : (Maybe String -> msg) -> Sub msg


port eventSource : (Json.Encode.Value -> msg) -> Sub msg


port scrolledToBottom : (Bool -> msg) -> Sub msg


port reportIsVisible : (( String, Bool ) -> msg) -> Sub msg


type Subscription
    = OnClockTick Interval
    | OnMouse
    | OnKeyDown
    | OnKeyUp
    | OnScrollToBottom
    | OnWindowResize
    | FromEventSource ( String, List String )
    | OnNonHrefLinkClicked
    | OnTokenReceived
    | OnElementVisible


type Delivery
    = KeyDown Int
    | KeyUp Int
    | Moused
    | ClockTicked Interval Time.Posix
    | ScrolledToBottom Bool
    | WindowResized Int Int
    | NonHrefLinkClicked String -- must be a String because we can't parse it out too easily :(
    | TokenReceived (Maybe String)
    | EventsReceived (Result Json.Decode.Error (List BuildEventEnvelope))
    | RouteChanged Routes.Route
    | ElementVisible ( String, Bool )


type Interval
    = OneSecond
    | FiveSeconds
    | OneMinute


runSubscription : Subscription -> Sub Delivery
runSubscription s =
    case s of
        OnClockTick t ->
            Time.every (intervalToTime t) (ClockTicked t)

        OnMouse ->
            Sub.batch
                [ onMouseMove (Json.Decode.succeed Moused)
                , onClick (Json.Decode.succeed Moused)
                ]

        OnKeyDown ->
            onKeyDown (Json.Decode.field "code" Json.Decode.int |> Json.Decode.map KeyDown)

        OnKeyUp ->
            onKeyUp (Json.Decode.field "code" Json.Decode.int |> Json.Decode.map KeyUp)

        OnScrollToBottom ->
            scrolledToBottom ScrolledToBottom

        OnWindowResize ->
            onResize WindowResized

        FromEventSource key ->
            eventSource
                (Json.Decode.decodeValue
                    (Json.Decode.list decodeBuildEventEnvelope)
                    >> EventsReceived
                )

        OnNonHrefLinkClicked ->
            newUrl NonHrefLinkClicked

        OnTokenReceived ->
            tokenReceived TokenReceived

        OnElementVisible ->
            reportIsVisible ElementVisible


intervalToTime : Interval -> Float
intervalToTime t =
    case t of
        OneSecond ->
            1000

        FiveSeconds ->
            5 * 1000

        OneMinute ->
            60 * 1000
