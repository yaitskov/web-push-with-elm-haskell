port module Main exposing (..)

import Browser

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

import Json.Decode as D
import Json.Encode as E

import Time

port putCache : E.Value -> Cmd msg
port cacheUpdated : (E.Value -> msg) -> Sub msg

-- port cacheUpdated : (D.Value -> msg) -> Sub msg

main = Browser.element
       { init = init
       , update = update
       , subscriptions = subscriptions
       , view = view
       }

type alias Model = Int
type Msg = Change String | Persist Model | Tick

init : () -> (Model, Cmd Msg)
init _ = (1, Cmd.none)

safeInt : String -> Maybe Int
safeInt s =
    case String.toInt s of
        Nothing -> Nothing
        Just n -> if n > 2^31
                  then Nothing
                  else Just n

doPersist : Model -> Cmd Msg
doPersist m = putCache <| E.int m

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick -> (model + 1, doPersist (model + 1))
        Change newVal ->
            let newModel = Maybe.withDefault model (safeInt newVal)
            in (newModel, doPersist newModel)
        Persist prevModel -> (prevModel, Cmd.none)

parseCache : E.Value -> Maybe Model
parseCache ev =
    case D.decodeValue D.int ev of
        Ok v -> Just v
        _ -> Nothing


subscriptions : Model -> Sub Msg
subscriptions originModel =
    Sub.batch
        [ cacheUpdated (\ev -> Persist (Maybe.withDefault originModel (parseCache ev)))
        , Time.every 1000 (\_ -> Tick)
        ]

view : Model -> Html Msg
view model =
    div []
        [ div [] [ text "HELLO WORLD" ]
        , div [] [ input
                       [ placeholder "content servives refresh"
                       , value (String.fromInt model)
                       , onInput Change
                       ]
                       []
                 ]
        ]
