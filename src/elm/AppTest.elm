module AppTest where

import ElmTest.Assertion exposing (..)
import ElmTest.Test exposing (..)

import App exposing (Model)
import Effects exposing (Effects)


addDigitSuite : Test
addDigitSuite =
  suite "Add digit Action Suite"
    [ test "Send first digit" (assertEqual "5" (.pincode <| fst(addDigit 5)))
    ]

addDigit : Int -> (App.Model, Effects App.Action)
addDigit val =
  App.update (App.AddDigit val) App.initialModel

all : Test
all =
  suite "All Event tests"
    [ addDigitSuite
    ]
