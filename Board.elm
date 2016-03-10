module Board where

import Cell
import CellCollection
import Player exposing (Player)
import Util

import Html exposing (..)
import Set


type alias Model =
  { cells : CellCollection.Model
  , size : ( Int , Int )
  }


up : ( Int , Int )
up = ( 0 , -1 )

down : ( Int , Int )
down = ( 0 , 1 )

left : ( Int , Int )
left = ( -1 , 0 )

right : ( Int , Int )
right = ( 1 , 0 )


update : CellCollection.Action -> Model -> Model
update action model =
  { model | cells = CellCollection.update action model.cells }


score : Player -> (List Cell.Model) -> Model -> Model
score player pieceCells oldBoard =
  let
    pieceColumns = Set.fromList <| List.map (\c -> fst c.pos) pieceCells

    columnCompleteAt : Int -> List Cell.Model -> Bool
    columnCompleteAt xPos boardCells =
      List.filter (\c -> (fst c.pos) == xPos) boardCells
        |> List.all (\c -> c.owner == player)

    removeColumnAt : Int -> List Cell.Model -> List Cell.Model
    removeColumnAt xPos boardCells =
      List.filter (\c -> (fst c.pos) /= xPos) boardCells

    moveCellsTowardsCenter : Int -> Player -> List Cell.Model -> List Cell.Model
    moveCellsTowardsCenter xPos player boardCells =
      let
        direction = case player of
          Player.Left  -> right
          Player.Right -> left
        onTheBrightSide cell = case player of
          Player.Left  -> (fst cell.pos) < xPos
          Player.Right -> (fst cell.pos) > xPos
        changePos cell =
          if onTheBrightSide cell then
            { cell | pos = ((fst cell.pos) + (fst direction), (snd cell.pos))  }
          else
            cell
      in
        List.map changePos boardCells

    addFirstColumn : Player -> (Int,Int) -> List Cell.Model -> List Cell.Model
    addFirstColumn player boardSize boardCells =
      let
        (w,h) = boardSize
        posX = case player of
          Player.Left  -> 1
          Player.Right -> w
        newCell posY = Cell.init (posX, posY) player
        newColumn = List.map newCell [1..h]
      in
        List.append boardCells newColumn

    removeColumnIfComplete : Int -> Model -> Model
    removeColumnIfComplete xPos board =
      if columnCompleteAt xPos board.cells then
        let newCells =
          removeColumnAt xPos board.cells
            |> moveCellsTowardsCenter xPos player
            |> addFirstColumn player board.size
        in
          { board | cells = newCells }
      else
        board
  in
    Set.foldr removeColumnIfComplete oldBoard pieceColumns


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
    { cells = List.map cellFor positions
    , size = ( w , h )
    }
