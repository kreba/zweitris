module Board exposing (..)

import Cell
import CellCollection
import Player exposing (Player)
import Util
import Html exposing (..)
import Set


type alias Model =
    { cells : CellCollection.Model
    , size : ( Int, Int )
    }


up : ( Int, Int )
up =
    ( 0, -1 )


down : ( Int, Int )
down =
    ( 0, 1 )


left : ( Int, Int )
left =
    ( -1, 0 )


right : ( Int, Int )
right =
    ( 1, 0 )


update : CellCollection.Msg -> Model -> Model
update msg model =
    { model | cells = CellCollection.update msg model.cells }


score : Player -> List Cell.Model -> Model -> Model
score player pieceCells oldBoard =
    let
        pieceColumns =
            Set.fromList <| List.map (\c -> Tuple.first c.pos) pieceCells

        columnCompleteAt : Int -> List Cell.Model -> Bool
        columnCompleteAt xPos boardCells =
            List.filter (\c -> (Tuple.first c.pos) == xPos) boardCells
                |> List.all (\c -> c.owner == player)

        removeColumnAt : Int -> List Cell.Model -> List Cell.Model
        removeColumnAt xPos boardCells =
            List.filter (\c -> (Tuple.first c.pos) /= xPos) boardCells

        moveCellsTowardsCenter : Int -> Player -> List Cell.Model -> List Cell.Model
        moveCellsTowardsCenter xPos player boardCells =
            let
                direction =
                    case player of
                        Player.Left ->
                            right

                        Player.Right ->
                            left

                onTheBrightSide cell =
                    case player of
                        Player.Left ->
                            (Tuple.first cell.pos) < xPos

                        Player.Right ->
                            (Tuple.first cell.pos) > xPos

                changePos cell =
                    if onTheBrightSide cell then
                        { cell | pos = ( (Tuple.first cell.pos) + (Tuple.first direction), (Tuple.second cell.pos) ) }
                    else
                        cell
            in
                List.map changePos boardCells

        addFirstColumn : Player -> ( Int, Int ) -> List Cell.Model -> List Cell.Model
        addFirstColumn player boardSize boardCells =
            let
                ( w, h ) =
                    boardSize

                posX =
                    case player of
                        Player.Left ->
                            1

                        Player.Right ->
                            w

                opposingPlayer =
                    case player of
                        Player.Left ->
                            Player.Right

                        Player.Right ->
                            Player.Left

                newCell posY =
                    Cell.init ( posX, posY ) opposingPlayer

                newColumn =
                    List.map newCell (List.range 1 h)
            in
                List.append boardCells newColumn

        removeColumnIfComplete : Int -> Model -> Model
        removeColumnIfComplete xPos board =
            if columnCompleteAt xPos board.cells then
                let
                    newCells =
                        removeColumnAt xPos board.cells
                            |> moveCellsTowardsCenter xPos player
                            |> addFirstColumn player board.size
                in
                    { board | cells = newCells }
            else
                board
    in
        Set.foldr removeColumnIfComplete oldBoard pieceColumns


view : Model -> Html CellCollection.Msg
view model =
    CellCollection.view model.cells


init : { w : Int, h : Int } -> Model
init { w, h } =
    let
        positions =
            Util.combinations (List.range 1 w) (List.range 1 h)

        cellFor pos =
            Cell.init pos (leftOrRight pos)

        leftOrRight pos =
            if Tuple.first pos > w // 2 then
                Player.Left
            else
                Player.Right
    in
        { cells = List.map cellFor positions
        , size = ( w, h )
        }
