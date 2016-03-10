module Player (Player(..), color) where

type Player
  = Left
  | Right

color : Player -> String
color player =
  case player of
    Left  -> "lightgray"
    Right -> "orange"
