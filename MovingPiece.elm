module MovingPiece where

import Cell
import CellCollection
import Player exposing (Player)

import Html exposing (..)
import Array
import Random

type alias Model  =
    { cells : CellCollection.Model
    }

update : CellCollection.Action -> Model -> Model
update action model =
  { model | cells = CellCollection.update action model.cells }

view : Signal.Address CellCollection.Action -> Model -> Html
view address model =
  CellCollection.view address model.cells

pieces : List (List Cell.Position)
pieces =
    [ [ (1,3),(2,3),(2,2),(2,1) ]         --   J
    , [ (1,2),(2,3),(2,2),(2,1) ]         --   -|
    , [ (1,1),(1,2),(1,3),(2,3) ]         --   L
    , [ (1,1),(1,2),(2,2),(2,3) ]         --   S
    , [ (2,1),(2,2),(1,2),(1,3) ]         --   Z
    , [ (1,1),(2,1),(1,2),(2,2) ]         --   O
    , [ (1,1),(1,2),(1,3),(1,4) ]         --   I
    ]

randomPiece : Random.Seed -> (List Cell.Position, Random.Seed)
randomPiece seed =
    let
        lastItem = List.length pieces - 1
        generator = Random.int 0 lastItem
        (randomIndex, seed) = (Random.generate generator seed) --(randInt, seed)
        getValue item =
          case item of
            Just i -> i
            Nothing -> Debug.crash "error: fromJust Nothing"
        piece =
            pieces
              |> Array.fromList
              |> Array.get randomIndex
              |> getValue
    in
        (piece, seed)


init : Player -> Random.Seed -> ( Model , Random.Seed )
init player seed =
  let
    (positions, nextSeed) = randomPiece seed
    cellFor pos =
        let
            (x, y) = pos
            pos =
                case player of
                    Player.Right -> (x + 30, y)
                    Player.Left  -> pos
        in
            Cell.init pos player
  in
    ( { cells = List.map cellFor positions } , nextSeed )


moveUp : Model -> Model
moveUp model =
  { model | cells = List.map (Cell.update (Cell.MoveBy (0, -1))) model.cells}


moveDown : Model -> Model
moveDown model =
  { model | cells = List.map (Cell.update (Cell.MoveBy (0, 1))) model.cells }
