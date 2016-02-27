module CellCollection where

import Cell

import Html exposing (..)
import Html.Attributes exposing (..)


-- MODEL

type alias Model = List Cell.Model


-- UPDATE

type Action
  = RelayToCell Cell.Position Cell.Action
  | MergeFrom Model
  | MoveBy Cell.Position
  | TurnCW

update : Action -> Model -> Model
update action model =
  case action of

    MergeFrom other ->
      List.map (mergeFrom other) model

    RelayToCell targetCellPos cellAction ->
      List.map (updateSingleCell targetCellPos cellAction) model

    MoveBy xy ->
      List.map (Cell.update (Cell.MoveBy xy)) model

    TurnCW ->
      let
        xMin = Maybe.withDefault 0 <| List.minimum <| List.map (.pos >> fst) model
        xMax = Maybe.withDefault 0 <| List.maximum <| List.map (.pos >> fst) model
        yMin = Maybe.withDefault 0 <| List.minimum <| List.map (.pos >> snd) model
        pieceSizeX = xMax - xMin

        updateCell : Cell.Model -> Cell.Model
        updateCell cell =
          let
            (relativeXold,relativeYold) = ( fst cell.pos - xMin , snd cell.pos - yMin )
            (relativeXnew,relativeYnew) = ( pieceSizeX - relativeYold , relativeXold )
            (dx,dy) = ( relativeXnew - relativeXold , relativeYnew - relativeYold )
          in
            Cell.update (Cell.MoveBy (dx,dy)) cell
      in
        List.map updateCell model

updateSingleCell : Cell.Position -> Cell.Action -> Cell.Model -> Cell.Model
updateSingleCell targetCellPos cellAction cell =
  if cell.pos == targetCellPos then
    Cell.update cellAction cell
  else
    cell

mergeFrom : List Cell.Model -> Cell.Model -> Cell.Model
mergeFrom newCells oldCell =
  case cellAt oldCell.pos newCells of
    Just newCell -> newCell
    Nothing      -> oldCell

cellAt : Cell.Position -> List Cell.Model -> Maybe Cell.Model
cellAt pos model =
  List.head <| List.filter (\a -> a.pos == pos) model


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
