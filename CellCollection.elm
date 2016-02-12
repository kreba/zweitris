module CellCollection where

import Cell

import Html exposing (..)
import Html.Attributes exposing (..)


-- MODEL

type alias Model = List Cell.Model


-- UPDATE

type Action
  = RelayToCell Cell.Position Cell.Action
  | MoveBy Cell.Position

update : Action -> Model -> Model
update action model =
  case action of

    RelayToCell targetCellPos cellAction ->
      List.map (updateSingleCell targetCellPos cellAction) model

    MoveBy xy ->
      List.map (Cell.update (Cell.MoveBy xy)) model


updateSingleCell : Cell.Position -> Cell.Action -> Cell.Model -> Cell.Model
updateSingleCell targetCellPos cellAction cell =
  if cell.pos == targetCellPos then
    Cell.update cellAction cell
  else
    cell


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div [ style [("position", "absolute")] ] (List.map (viewCell address) model)


viewCell : Signal.Address Action -> Cell.Model -> Html
viewCell address cell =
  let
    relayAddress = Signal.forwardTo address (RelayToCell cell.pos)
  in
    Cell.view relayAddress cell
