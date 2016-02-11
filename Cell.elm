module Cell where

import Player exposing (Player)

import Html exposing (..)
import Html.Attributes exposing (id, class, style)


-- MODEL

type alias Position = ( Int , Int )

type alias Model = { pos : Position , owner : Player }


init : Position -> Player -> Model
init position player = { pos = position , owner = player }


-- UPDATE

type Action
  = SetPlayer Player
  | MoveByXY Position

update : Action -> Model -> Model
update action model =
  case action of
    SetPlayer player -> { model | owner = player }
    MoveByXY (x, y) -> {model | pos = (fst model.pos + x, snd model.pos + y)}


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  let
    posX = fst model.pos
    posY = snd model.pos
    cellSize = 50
    elementStyle =
      [ ("background-color", Player.color model.owner)
      , ("box-sizing", "border-box")
      , ("width", toString cellSize ++ "px")
      , ("height", toString cellSize ++ "px")
      , ("padding", "3px")
      , ("border", "1px solid white")
      , ("position", "absolute")
      , ("left", toString (posX * cellSize) ++ "px")
      , ("top", toString (posY * cellSize) ++ "px")
      ]
  in
    div
      [ class "cell"
      , id ("cell-" ++ toString posX ++ "-" ++ toString posY)
      , style elementStyle
      ]
      [ text (toString posX ++ ", " ++ toString posY) ]
