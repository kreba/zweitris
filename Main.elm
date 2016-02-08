import Cell
import Player
import StartApp exposing (start)
import Html exposing (..)
import Keyboard
import Effects

type alias CellId = Int

type Action
  = RelayToCell CellId Cell.Action
  | TogglePause
  | Reset
  | Noop

type alias Model =
  { cells : List (CellId, Cell.Model)
  , paused : Bool
  }

initialModel : Model
initialModel =
  { cells = List.map (\id -> ( id , Cell.init Player.Left )) [1..5]
  , paused = False
  }

main : Signal Html
main =
  app.html

app : StartApp.App Model
app =
  start
    { init = ( initialModel , Effects.none )
    , update = update
    , view = view
    , inputs = [ Signal.map togglePause Keyboard.space ]
    }


togglePause : Bool -> Action
togglePause spacePressed =
  if spacePressed then
    TogglePause
  else
    Noop


update : Action -> Model -> ( Model , Effects.Effects Action )
update action oldModel =
  let newModel = case action of

      RelayToCell cellId cellAction ->
        relayUpdateTo cellId cellAction oldModel

      TogglePause ->
        { oldModel | paused = not oldModel.paused }

      Reset ->
        initialModel

      Noop ->
        oldModel

  in
    ( newModel, Effects.none )

relayUpdateTo : CellId -> Cell.Action -> Model -> Model
relayUpdateTo targetCellId cellAction oldModel =
  let updateCell ( cellId , oldCellModel ) =
        if cellId == targetCellId then
          ( cellId , Cell.update cellAction oldCellModel )
        else
          ( cellId , oldCellModel )
  in
    { oldModel | cells = List.map updateCell oldModel.cells }


view : Signal.Address Action -> Model -> Html
view address model =
  let
    info = [ text (if model.paused then "PAUSED" else "RUNNING") ]
    board = List.map (viewCell address) model.cells
  in
    div [] (info ++ board)

viewCell : Signal.Address Action -> (CellId, Cell.Model) -> Html
viewCell address (cellId, cellModel) =
  Cell.view (relaySignalFrom cellId address) cellModel

relaySignalFrom : CellId -> Signal.Address Action -> Signal.Address Cell.Action
relaySignalFrom cellId address =
  Signal.forwardTo address (RelayToCell cellId)
