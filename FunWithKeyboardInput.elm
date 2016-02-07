module FunWithKeyboardInput () where

import Player exposing (Player)
import StartApp
import Html exposing (div, button, text)
import Html.Events exposing (onClick)
import Effects
import Task
import Keyboard

type alias Model = Int

type Action
  = Increment Int
  | Decrement Int
  | NoOp
  | Set Int


main : Signal Html.Html
main =
  app.html


app : StartApp.App Model
app =
  StartApp.start
    { init = init 0
    , view = view
    , update = update
    , inputs =
      [ Signal.map (playerAction Left) Keyboard.wasd
      , Signal.map (playerAction Right) Keyboard.arrows
      , Signal.map resetAction Keyboard.space
      ]
    }


init : Int -> ( Model, Effects.Effects a )
init i =
  (i, Effects.none)


view : Signal.Address Action -> a -> Html.Html
view address model =
  div []
    [ button [ onClick address (Increment 1) ] [ text "+" ]
    , div [] [ text (toString model) ]
    , button [ onClick address (Decrement 1) ] [ text "-" ]
    ]


update : Action -> Model -> ( Model, Effects.Effects a )
update action oldModel =
  let
    newModel =
      case action of
        NoOp          -> oldModel
        Increment int -> oldModel + int
        Decrement int -> oldModel - int
        Set int       -> int
  in
    ( newModel, Effects.none )

playerAction : Player -> { x:Int, y:Int } -> Action
playerAction player {x,y} =
  let
    times =
      case player of
        Left  -> 10
        Right ->  1
  in
    case (x, y) of
        (_,  1) -> Increment times
        (_, -1) -> Decrement times
        (_,  _) -> NoOp

resetAction : Bool -> Action
resetAction spaceDown =
  if spaceDown then
    Set 88
  else
    Set 0


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks
