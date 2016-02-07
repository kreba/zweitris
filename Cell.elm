module Cell (Model, init, Action, update, view) where

import Player exposing (Player)
import Debug
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


-- MODEL

type alias Model = String


init : String -> Model
init color = color


-- UPDATE

type Action = SetPlayer Player

update : Action -> Model -> Model
update action model =
  case action of
    SetPlayer player -> player

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div [ cellStyle ] []

cellStyle : Attribute
cellStyle =
  style
    [ ("background-color", "lightblue")
    , ("width", "50px")
    , ("height", "50px")
    ]
