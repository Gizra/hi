module Hi where

import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import String exposing (length, repeat)


-- MODEL

type alias Model =
  { pinCode : String
  }

initialModel : Model
initialModel =
  { pinCode = ""
  }

init : (Model, Effects Action)
init =
  ( initialModel
  , Effects.none
  )


-- UPDATE

type Action = AddDigit Int

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    AddDigit digit ->
    ( { model |
          pinCode <-
            if length model.pinCode < 4
              then model.pinCode ++ toString(digit)
              else model.pinCode
      }
    , Effects.none
    )

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div [class "number-pad"]
    [ div [class "number-buttons"]
    -- ( List.map digitButton [1..9] )
    [ button [ onClick address (AddDigit 1) ] [ text "1" ] ]
    , div [ ] [ text <| repeat (length model.pinCode) "*" ]
    ]


digitButton : Int -> Html
digitButton digit =
  button [ ] [ text <| toString digit ]