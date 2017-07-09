module CellCollection exposing (..)

import Cell
import Html exposing (..)
import Html.Attributes exposing (..)


-- MODEL


type alias Model =
    List Cell.Model



-- UPDATE


type Msg
    = RelayToCell Cell.Position Cell.Msg
    | MergeFrom Model
    | Move Cell.Position
    | Rotate


update : Msg -> Model -> Model
update msg model =
    case msg of
        MergeFrom other ->
            List.map (mergeFrom other) model

        RelayToCell targetCellPos cellMsg ->
            List.map (updateSingleCell targetCellPos cellMsg) model

        Move xy ->
            List.map (Cell.update (Cell.MoveBy xy)) model

        Rotate ->
            let
                xMin =
                    Maybe.withDefault 0 <| List.minimum <| List.map (.pos >> Tuple.first) model

                xMax =
                    Maybe.withDefault 0 <| List.maximum <| List.map (.pos >> Tuple.first) model

                yMin =
                    Maybe.withDefault 0 <| List.minimum <| List.map (.pos >> Tuple.second) model

                tetriminoSizeX =
                    xMax - xMin

                updateCell : Cell.Model -> Cell.Model
                updateCell cell =
                    let
                        ( relativeXold, relativeYold ) =
                            ( Tuple.first cell.pos - xMin, Tuple.second cell.pos - yMin )

                        ( relativeXnew, relativeYnew ) =
                            ( tetriminoSizeX - relativeYold, relativeXold )

                        ( dx, dy ) =
                            ( relativeXnew - relativeXold, relativeYnew - relativeYold )
                    in
                        Cell.update (Cell.MoveBy ( dx, dy )) cell
            in
                List.map updateCell model


updateSingleCell : Cell.Position -> Cell.Msg -> Cell.Model -> Cell.Model
updateSingleCell targetCellPos cellMsg cell =
    if cell.pos == targetCellPos then
        Cell.update cellMsg cell
    else
        cell


mergeFrom : List Cell.Model -> Cell.Model -> Cell.Model
mergeFrom newCells oldCell =
    case cellAt oldCell.pos newCells of
        Just newCell ->
            newCell

        Nothing ->
            oldCell


cellAt : Cell.Position -> List Cell.Model -> Maybe Cell.Model
cellAt pos model =
    List.head <| List.filter (\a -> a.pos == pos) model



-- VIEW


view : Model -> Html Msg
view model =
    div [ style [ ( "position", "absolute" ) ] ] (List.map viewCell model)


viewCell : Cell.Model -> Html Msg
viewCell cell =
    Html.map (RelayToCell cell.pos) (Cell.view cell)
