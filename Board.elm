module Board where

import Cell
import CellCollection
import Player exposing (Player)
import Util

import Html exposing (..)


type alias Model  = { cells : CellCollection.Model }

update : CellCollection.Action -> Model -> Model
update action model =
  { model | cells = CellCollection.update action model.cells }

view : Signal.Address CellCollection.Action -> Model -> Html
view address model =
  CellCollection.view address model.cells


init : { w : Int , h : Int } -> Model
init {w,h} =
  let
    positions = Util.combinations [1..w] [1..h]
    cellFor pos = Cell.init pos (leftOrRight pos)
    leftOrRight pos = if fst pos > w // 2 then Player.Left else Player.Right
  in
    { cells = List.map cellFor positions }
