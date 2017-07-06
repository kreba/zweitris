module Player exposing (Player(..), color)


type Player
    = Left
    | Right


color : Player -> String
color player =
    case player of
        Left ->
            "lightgray"

        Right ->
            "orange"
