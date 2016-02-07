import Cell
import Player
import StartApp exposing (start)
import Html exposing (..)
import Keyboard
import Effects


type alias Model = List Cell.Model

type Action
  = SetLeft
  | SetRight

main : Signal Html
main =
  app.html


app : StartApp.App Model
app =
  start
    { init = ( [ Cell.init Player.left ] , Effects.none )
    , update = update
    , view = view
    , inputs = [ Signal.map toggleCell Keyboard.space ]
    }

toggleCell : Bool -> Action
toggleCell spacePressed =
  if spacePressed then
    SetLeft
  else
    SetRight

update : Action -> Model -> ( Model , Effects.Effects Action )
update action oldModel =
  let
    newModel = List.map (convert action) oldModel
  in
    ( newModel, Effects.none )

convert : Action -> Cell.Model -> Cell.Model
convert action cell =
  case action of
    SetLeft  -> Cell.init Player.left
    SetRight -> Cell.init Player.right


view : Signal.Address Action -> Model -> Html
view address model =
  div [] (List.map (\cell -> Cell.view address cell) model)
