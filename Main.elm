import Cell
import Player
import StartApp exposing (start)
import Html exposing (..)
import Keyboard
import Effects

type Action
  = RelayToCell Cell.Id Cell.Action
  | TogglePause
  | Reset
  | Noop

type alias Model =
  { cells : List Cell.Model
  , paused : Bool
  }

initialModel : Model
initialModel =
  { cells = List.map (\id -> Cell.init id Player.Left) [1..5]
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
togglePause spaceDown =
  if spaceDown then
    TogglePause
  else
    Noop


update : Action -> Model -> ( Model , Effects.Effects Action )
update action oldModel =
  let newModel = case action of

      RelayToCell cellId cellAction ->
        { oldModel | cells = List.map (updateSingleCell cellId cellAction) oldModel.cells }

      TogglePause ->
        { oldModel | paused = not oldModel.paused }

      Reset ->
        initialModel

      Noop ->
        oldModel

  in
    ( newModel, Effects.none )


updateSingleCell : Cell.Id -> Cell.Action -> Cell.Model -> Cell.Model
updateSingleCell targetCellId cellAction cell =
  if cell.id == targetCellId then
    Cell.update cellAction cell
  else
    cell

view : Signal.Address Action -> Model -> Html
view address model =
  let
    info = [ text (if model.paused then "PAUSED" else "RUNNING") ]
    board = List.map (viewCell address) model.cells
  in
    div [] (info ++ board)


viewCell : Signal.Address Action -> Cell.Model -> Html
viewCell address cell =
  let
    relayAddress = Signal.forwardTo address (RelayToCell cell.id)
  in
    Cell.view relayAddress cell
