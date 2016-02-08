module Cell (Model, init, Action(SetPlayer), update, view) where

import Player exposing (Player)
import Html exposing (..)
import Html.Attributes exposing (style)


-- MODEL

type alias Model = String


init : Player -> Model
init player = Player.color player


-- UPDATE

type Action = SetPlayer Player

update : Action -> Model -> Model
update action model =
  case action of
    SetPlayer player -> Player.color player


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div [ cellStyle model ] []

cellStyle : String -> Attribute
cellStyle color =
  style
    [ ("background-color", color)
    , ("width", "50px")
    , ("height", "50px")
    ]
