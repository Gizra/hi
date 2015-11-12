module Hi where

import Config exposing (backendUrl)
import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Encode as JE exposing (string, Value)
import Json.Decode as JD exposing ((:=))
import String exposing (length)
import Task

import Debug

-- MODEL

type Status =
  Init
  | Fetching
  | Fetched
  | HttpError Http.Error

type alias Model =
  { pincode : String
  , status : Status
  }

initialModel : Model
initialModel =
  { pincode = ""
  , status = Init
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
  | ShowResponse (Result Http.Error String)

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
        ( { model | pincode <- pincode' }
        , effects'
        )

    SubmitCode ->
      let
        url : String
        url = Config.backendUrl ++ "/api/"

      in
        if model.status == Fetching || model.status == Fetched
          then
            (model, Effects.none)
          else
            ( { model
              | status <- Fetching
              }
            , getJson url "hi"
            )


    ShowResponse result ->
      case result of
        Ok token ->
          ( { model | status <- Fetched }
          , Effects.none
          )
        Err msg ->
          ( { model | status <- HttpError msg }
          , Effects.none
          )
-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div
    [ class "keypad" ]
    [ div
      [ class "number-buttons" ]
      ( List.map (digitButton address) [0..9] |> List.reverse )
    ]

digitButton : Signal.Address Action -> Int -> Html
digitButton address digit =
  button [ onClick address (AddDigit digit) ] [ text <| toString digit ]


-- EFFECTS


getJson : String -> String -> Effects Action
getJson url credentials =
  Http.send Http.defaultSettings
    { verb = "GET"
    , headers = []
    , url = url
    , body = Http.empty
    }
    |> Http.fromJson decodePincode
    |> Task.toResult
    |> Task.map ShowResponse
    |> Effects.task


decodePincode : JD.Decoder String
decodePincode =
  JD.at ["pincode"] <| JD.string
