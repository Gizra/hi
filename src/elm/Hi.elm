module Hi where

import Config exposing (backendUrl)
import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Json exposing ((:=))
import Json.Encode as JE
import String exposing (length)
import Task

import Debug

-- MODEL

type Message =
  Empty
  | Error String
  | Success String

type Status =
  Init
  | Fetching
  | Fetched
  | HttpError Http.Error

type alias Response =
  { employee : String
  , action : String
  }

type alias Model =
  { pincode : String
  , status : Status
  , message : Message
  }

initialModel : Model
initialModel =
  { pincode = ""
  , status = Init
  , message = Empty
  }

init : (Model, Effects Action)
init =
  ( initialModel
  , Effects.none
  )


-- UPDATE

type Action
  = AddDigit Int
  | SubmitCode
  | ShowResponse (Result Http.Error Response)
  | SetMessage Message

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    AddDigit digit ->
      let
        pincode' =
          if length model.pincode < 4
            then model.pincode ++ toString(digit)
            else ""
        effects' =
          if length model.pincode == 3
            then Task.succeed SubmitCode |> Effects.task
            else Effects.none
      in
        ( { model
          | pincode <- pincode'
          , message <- Empty
          }
        , effects'
        )

    SubmitCode ->
      let
        url : String
        url = Config.backendUrl ++ "/api/v1.0/session"
      in
        if model.status == Fetching || model.status == Fetched
          then
            (model, Effects.none)
          else
            ( { model
              | pincode <- ""
              , status <- Fetching
              }
            , getJson url model.pincode
            )


    ShowResponse result ->
      case result of
        Ok session ->
          ( { model | status <- Fetched }
          , Task.succeed (SetMessage (Success "Success")) |> Effects.task
          )
        Err error ->
          let
            message =
              getErrorMessageFromHttpResponse error
          in
            ( { model | status <- HttpError error }
            , Task.succeed (SetMessage <| Error message) |> Effects.task
            )

    SetMessage message ->
      ( { model | message <- message }
      , Effects.none
      )

getErrorMessageFromHttpResponse : Http.Error -> String
getErrorMessageFromHttpResponse error =
  case error of
    Http.Timeout ->
      "Connection has timed out"

    Http.BadResponse code _ ->
      if | code == 401 -> "Wrong username or password"
         | code == 429 -> "Too many login requests with the wrong username or password. Wait a few hours before trying again"
         | code >= 500 -> "Some error has occured on the server"
         | otherwise -> "Unknow error has occured"

    Http.NetworkError ->
      "A network error has occured"

    Http.UnexpectedPayload message ->
      "Unexpected response: " ++ message

    _ ->
      "Unexpected error: " ++ toString error


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div
    [ class "keypad" ]
    [ div
        [ class "number-buttons" ]
        ( List.map (digitButton address) [0..9] |> List.reverse )
    , (viewMessage model.message)
    ]

viewMessage : Message -> Html
viewMessage message =
  let
    (className, string) =
      case message of
        Empty -> ("", "")
        Error msg -> ("error", msg)
        Success msg -> ("success", msg)
  in
    div [ id "status-message", class className ] [ text string ]


digitButton : Signal.Address Action -> Int -> Html
digitButton address digit =
  button [ onClick address (AddDigit digit) ] [ text <| toString digit ]


-- EFFECTS

getJson : String -> String -> Effects Action
getJson url pincode =
  Http.send Http.defaultSettings
    { verb = "POST"
    , headers = [ ("access-token", "BgIoWiCd_5WwC4R3tq3tjdBUPUPYZtUYA0AZObkFMBg") ]
    , url = url
    , body = ( Http.string <| dataToJson pincode )
    }
    |> Http.fromJson decodeResponse
    |> Task.toResult
    |> Task.map ShowResponse
    |> Effects.task

dataToJson : String -> String
dataToJson code =
  JE.encode 0
    <| JE.object
        [ ("pincode", JE.string code) ]

decodeResponse : Json.Decoder Response
decodeResponse =
  Json.at ["data"]
    <| Json.object2 Response
      ("employee" := Json.string)
      ("action" := Json.string)
