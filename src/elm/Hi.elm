module Hi where

import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http exposing (Error)
import String exposing (length, repeat)
import Task exposing (Task, succeed)

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
        (pinCode', effects) =
          if length model.pinCode < 4
            then
              ( model.pinCode ++ toString(digit)
              , Effects.none
              )
            else
              ( model.pinCode
              , Task.succeed SubmitCode |> Effects.task
              )
      in
        ( { model | pinCode <- pinCode' }
        , effects
        )

    SubmitCode ->
      let
        d = Debug.log "SubmitCode" True
      in
      ( model, Effects.none )

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  let
    digitButton : Int -> Html
    digitButton digit =
      button [ onClick address (AddDigit digit) ] [ text <| toString digit ]
  in
  div [class "number-pad"]
    [ div
        [ class "number-buttons" ]
        (List.map digitButton [1..9])



    , div [ ] [ text <| repeat (length model.pinCode) "*" ]
    ]
