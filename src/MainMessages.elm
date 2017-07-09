module MainMessages exposing (Msg(..))

import MainModel exposing (KeyDown)
import CellCollection
import Player exposing (Player)
import Time exposing (Time)


type Msg
    = Tick Time
    | TogglePause
    | Keypress KeyDown Bool
    | RelayToBoard CellCollection.Msg
    | RelayToMP Player CellCollection.Msg
    | Reset
    | Noop
