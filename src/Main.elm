port module Main exposing (..)

import Browser

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

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
      }

type Model = Model Int WebPushState

type Msg = Change String
         | Persist Int
         | CloseSubscription
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
                 Ok [p256dh, auth] -> Just (Wp { p256dh = p256dh,
                                                     auth = auth })
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
                       Wp {auth, p256dh} -> div
                         [ Html.Attributes.style "border" "1px solid green" ]
                         [ div [] [
                              button [ onClick CloseSubscription ]
                                  [ text "Close Subscription" ]
                             ]
                         , div [] [
                              text ("WebPush subscription [" ++ auth ++ "] [" ++ p256dh ++ "]")
                              ]
                         ]
                 ]
        ]
