module Hi where

import Config
import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http exposing (Error, get)
import String exposing (length, repeat)
import Task exposing (Task, succeed)
import Json.Decode as Json exposing ((:=))

import Debug

-- MODEL

type Status =
  Init
  | Fetching
  | Fetched
  | HttpError Http.Error

type alias Model =
  { pinCode : String
  , status : Status
  }

initialModel : Model
initialModel =
  { pinCode = ""
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

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    AddDigit digit ->
      let
        pinCode' =
          if length model.pinCode < 4
            then model.pinCode ++ toString(digit)
            else ""
        effects' =
          if length model.pinCode == 3
            then Task.succeed SubmitCode |> Effects.task
            else Effects.none
      in
        ( { model | pinCode <- pinCode' }
        , effects'
        )

    SubmitCode ->
      let
        _ = Debug.log model.pinCode True
--        url : String
--        url = Config.backendUrl ++ "/api/clock/in"

      in
        ( { model | pinCode <- "" }
        , lookupZipCode "123"
        |> Task.toResult
        |> Effects.task)

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


lookupZipCode : String -> Task Http.Error (List String)
lookupZipCode query =
  Http.get places ("http://api.zippopotam.us/us/" ++ query)


places : Json.Decoder (List String)
places =
  let place =
    Json.object2 (\city state -> city ++ ", " ++ state)
        ("place name" := Json.string)
        ("state" := Json.string)
  in
    "places" := Json.list place