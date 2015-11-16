module Hi where

import Config exposing (backendUrl)
import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD
import Json.Encode as JE
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
        url = Config.backendUrl ++ "/api/v1.0/session?access_token=" ++ "lXlTh7PR30mQN316SN3LofK95krQjCltBnygfjkleyQ"

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
getJson url pincode =
  Http.post
    decodeAccessToken
    (url)
    (Http.string <| dataToJson pincode )
    |> Task.toResult
    |> Task.map ShowResponse
    |> Effects.task


dataToJson : String -> String
dataToJson code =
  JE.encode 0
    <| JE.object
        [ ("pincode", JE.string code) ]

decodeAccessToken : JD.Decoder String
decodeAccessToken =
  JD.at ["access_token"] <| JD.string


{--
getJson : String -> String -> Effects Action
getJson url credentials =
  Http.send Http.defaultSettings
    { verb = "POST"
    , headers = [ ("access-token", "lXlTh7PR30mQN316SN3LofK95krQjCltBnygfjkleyQ") ]
    , url = url
    , body = Http.string <| JE.encode 0
      <| JE.object
          [ ("pincode", "5555") ]
    }
    |> Http.fromJson decodePincode
    |> Task.toResult
    |> Task.map ShowResponse
    |> Effects.task


decodePincode : Json.Decode.Decoder String
decodePincode =
  Json.Decode.at ["pincode"] <| Json.Decode.string
--}