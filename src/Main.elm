port module Main exposing (..)

import Browser

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

import Http

import Json.Decode as D
import Json.Encode as E

import Time

port putCache : E.Value -> Cmd msg
port cacheUpdated : (E.Value -> msg) -> Sub msg
port serviceWorkerWasNotRegistered : (E.Value -> msg) -> Sub msg
port serviceWorkerIsRegistered : (E.Value -> msg) -> Sub msg
port newWebPushSubscription : (E.Value -> msg) -> Sub msg
port checkWebPush : () -> Cmd msg -- not supported | declined | not established | established
port tryWebPush : () -> Cmd msg -- new p256h auth if success
port closeWebPushSubscription : () -> Cmd msg

main = Browser.element
       { init = init
       , update = update
       , subscriptions = subscriptions
       , view = view
       }


type WebPushState
    = WpNotSupported
    | WpX
    | WpSubscribing
    | WpNotGranted
    | WpExpired
    | Wp
      { p256dh : String
      , auth : String
      , serverKnows : Bool
      , broadcastWebPushMessage : String
      }

type Model = Model Int WebPushState

type Msg = Change String
         | Persist Int
         | BroadcastMsgModified String
         | BroadcastOverWebPush
         | CloseSubscription
         | SendSubscriptionToServer
         | ServerWpBindResponse (Result Http.Error ())
         | BroadcastServerWpResponse (Result Http.Error ())
         | TryNewWpSubscription
         | ServiceWorkerIsRegistered
         | ServiceWorkerWasNotRegistered
         | NewWebPushState (Maybe WebPushState)
         | Tick

init : () -> (Model, Cmd Msg)
init _ = (Model 1 WpX, Cmd.none)

safeInt : String -> Maybe Int
safeInt s =
    case String.toInt s of
        Nothing -> Nothing
        Just n -> if n > 2^31
                  then Nothing
                  else Just n

doPersist : Int -> Cmd Msg
doPersist m = putCache <| E.int m

update : Msg -> Model -> (Model, Cmd Msg)
update msg (Model model wp) =
    case msg of
        Tick -> (Model (model + 1) wp, doPersist (model + 1))
        Change newVal ->
            let newModel = Maybe.withDefault model (safeInt newVal)
            in (Model newModel wp, doPersist newModel)
        Persist prevModel -> (Model prevModel wp, Cmd.none)
        CloseSubscription -> (Model model wp, closeWebPushSubscription ())
        ServiceWorkerWasNotRegistered -> (Model model WpNotSupported, Cmd.none)
        ServiceWorkerIsRegistered -> (Model model wp, checkWebPush ())
        BroadcastMsgModified newBroadcastMsg ->
            case wp of
                Wp {auth, p256dh, serverKnows} ->
                    (Model model (Wp { auth = auth
                                     , p256dh = p256dh
                                     , serverKnows = serverKnows
                                     , broadcastWebPushMessage = newBroadcastMsg
                                     }),
                         Cmd.none)
                _ -> (Model model wp, Cmd.none)

        BroadcastOverWebPush ->
            case wp of
                Wp {broadcastWebPushMessage} ->
                    (Model model wp,
                         Http.post
                         { url = "/broadcast-msg-over-web-push"
                         , body = Http.jsonBody (E.string broadcastWebPushMessage)
                         , expect = Http.expectWhatever BroadcastServerWpResponse
                         })
                _ -> (Model model wp, Cmd.none)

        ServerWpBindResponse r ->
            case r of
                Ok _ ->
                    case wp of
                        Wp {auth, p256dh, broadcastWebPushMessage} ->
                            (Model model (Wp { auth = auth
                                             , p256dh = p256dh
                                             , serverKnows = True
                                             , broadcastWebPushMessage = broadcastWebPushMessage
                                             }),
                                 Cmd.none)
                        _ -> (Model model wp, Cmd.none)
                Err _ -> (Model model wp, Cmd.none)

        BroadcastServerWpResponse res ->
            case wp of
                Wp {auth, p256dh, broadcastWebPushMessage, serverKnows} ->
                    case res of
                        Ok _ -> (Model model
                                     (Wp { broadcastWebPushMessage = ""
                                        , auth = auth
                                        , p256dh = p256dh
                                        , serverKnows = serverKnows
                                        }),
                                       Cmd.none)
                        _ -> (Model model wp, Cmd.none)
                _ -> (Model model wp, Cmd.none)

        SendSubscriptionToServer ->
            case wp of
                Wp {p256dh, auth} ->
                    (Model model wp,
                         Http.post
                         { url = "/web-push-subscription/p256dh/" ++ p256dh ++ "/auth/" ++ auth
                         , body = Http.emptyBody
                         , expect = Http.expectWhatever ServerWpBindResponse
                         })
                _ -> (Model model wp, Cmd.none)
        TryNewWpSubscription ->
            (Model model WpSubscribing, tryWebPush ())
        NewWebPushState newWpSt ->
            (Model model (Maybe.withDefault wp newWpSt), Cmd.none)


parseCache : E.Value -> Maybe Int
parseCache ev =
    case D.decodeValue D.int ev of
        Ok v -> Just v
        _ -> Nothing

parseWebPushState : E.Value -> Maybe WebPushState
parseWebPushState ev =
    case D.decodeValue D.string ev of
        Ok s -> case s of
                    "WpNotSupported" -> Just WpNotSupported
                    "WpX" -> Just WpX
                    "WpSubscribing" ->  Just WpSubscribing
                    "WpNotGranted" -> Just WpNotGranted
                    "WpExpired" -> Just WpExpired
                    _ -> Nothing
        _ -> case D.decodeValue (D.list D.string) ev of
                 Ok [p256dh, auth] -> Just (Wp { p256dh = p256dh
                                               , auth = auth
                                               , serverKnows = False
                                               , broadcastWebPushMessage = ""
                                               })
                 _ -> Nothing


subscriptions : Model -> Sub Msg
subscriptions (Model originModel _) =
    Sub.batch
        [ cacheUpdated (\ev -> Persist (Maybe.withDefault originModel (parseCache ev)))
        , newWebPushSubscription (\wps -> NewWebPushState (parseWebPushState wps))
        , Time.every 1000 (\_ -> Tick)
        , serviceWorkerIsRegistered (\_ -> ServiceWorkerIsRegistered)
        , serviceWorkerWasNotRegistered (\_ -> ServiceWorkerWasNotRegistered)
        ]

view : Model -> Html Msg
view (Model model wp) =
    div []
        [ div [] [ text "HELLO WORLD" ]
        , div [] [ input
                       [ placeholder "content servives refresh"
                       , value (String.fromInt model)
                       , onInput Change
                       ]
                       []
                 ]
        , div [] [ case wp of
                       WpX -> text "Checking WebPush Status..."
                       WpNotSupported -> text "WebPush is not supported by the browser"
                       WpNotGranted -> text "WebPush is disabled by user"
                       WpSubscribing -> text "Establishing web push subscription..."
                       WpExpired -> div []
                                    [ text "Subscription expired or missing"
                                    , button
                                          [ onClick TryNewWpSubscription ]
                                          [ text "Subscribe" ]
                                    ]
                       Wp {auth, p256dh, serverKnows, broadcastWebPushMessage} -> div
                         [ Html.Attributes.style "border" "1px solid green" ]
                         [ div [] [
                                button [ onClick CloseSubscription ]
                                    [ text "Close Subscription" ]
                               ]
                         , div [Html.Attributes.hidden (not serverKnows)]
                             [ div []
                                   [ text "Message:"
                                   , input [ onInput BroadcastMsgModified
                                           , value broadcastWebPushMessage] []
                                   ]
                             , button [ onClick BroadcastOverWebPush ] [ text "broadcast" ]
                             ]
                         , div [hidden serverKnows] [
                                button [ onClick SendSubscriptionToServer ]
                                    [ text "Send subscrition to server" ]
                               ]
                         , div [] [
                              text ("WebPush subscription [" ++ auth ++ "] [" ++ p256dh ++ "]")
                              ]
                         ]
                 ]
        ]
