module MovingPiece where

import Cell
import CellCollection
import Player exposing (Player)

import Html exposing (..)

type alias Model  = { cells : CellCollection.Model }

update : CellCollection.Action -> Model -> Model
update action model =
  { model | cells = CellCollection.update action model.cells }

view : Signal.Address CellCollection.Action -> Model -> Html
view address model =
  CellCollection.view address model.cells


init : List Cell.Position -> Player -> Model
init positions player =
  let
    cellFor pos = Cell.init pos player
  in
    { cells = List.map cellFor positions }
