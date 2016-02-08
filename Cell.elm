module Cell (Id, Model, init, Action(SetPlayer), update, view) where

import Player exposing (Player)
import Html exposing (..)
import Html.Attributes exposing (style)


-- MODEL

type alias Id = Int

type alias Model = { id : Id , owner : Player }


init : Id -> Player -> Model
init id player = { id = id , owner = player }


-- UPDATE

type Action = SetPlayer Player

update : Action -> Model -> Model
update action model =
  case action of
    SetPlayer player -> { model | owner = player }


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  let
    cellStyle =
      style
        [ ("background-color", Player.color model.owner)
        , ("width", "1em")
        , ("height", "1em")
        , ("padding", "1em")
        , ("border", "1px solid gray")
        ]
  in
    div [ cellStyle ] [ text <| toString model.id ]
