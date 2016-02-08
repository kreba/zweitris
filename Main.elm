import Cell
import Player
import StartApp exposing (start)
import Html exposing (..)
import Html.Attributes exposing (style)
import Keyboard
import Effects


type Action
  = RelayToCell Cell.Position Cell.Action
  | TogglePause
  | Reset
  | Noop


type alias Model =
  { cells : List Cell.Model
  , paused : Bool
  }


initialModel : Model
initialModel =
  { cells = initialBoard { w = 20 , h = 8 }
  , paused = False
  }


initialBoard : { w : Int , h : Int } -> List Cell.Model
initialBoard {w,h} =
  let
    positions = combinations [1..w] [1..h]
    cellFor pos = Cell.init pos (leftOrRight pos)
    leftOrRight pos = if fst pos > w // 2 then Player.Left else Player.Right
  in
    List.map cellFor positions


{-| Takes two lists and returns all possible combinations of their elements,
    i.e. their cartesian product.
-}
combinations : List a -> List b -> List ( a , b )
combinations xs ys =
  List.concat (List.map (\x -> List.map (\y -> (x, y)) ys) xs)

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


updateSingleCell : Cell.Position -> Cell.Action -> Cell.Model -> Cell.Model
updateSingleCell targetCellPos cellAction cell =
  if cell.pos == targetCellPos then
    Cell.update cellAction cell
  else
    cell


view : Signal.Address Action -> Model -> Html
view address model =
  let
    info = text (if model.paused then "PAUSED" else "RUNNING")
    board = div [ style [("position", "absolute")] ] (List.map (viewCell address) model.cells)
  in
    div [] [ info , board ]


viewCell : Signal.Address Action -> Cell.Model -> Html
viewCell address cell =
  let
    relayAddress = Signal.forwardTo address (RelayToCell cell.pos)
  in
    Cell.view relayAddress cell
