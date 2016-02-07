module Body (Model, init, update, view) where

-- import Debug
import Cell exposing (Action)
import Html exposing (..)
import Html.Attributes exposing (style)
-- import Html.Events exposing (onClick)



-- MODEL

type alias Model = List Cell.Model


init : Model
init = List.repeat 5 (Cell.init "lightgreen")


-- UPDATE
type Action = Click

-- update : Action -> Model -> Model
-- update action model = List.map Cell.update (action model)
--
--
-- -- VIEW
--
-- view : Signal.Address Action -> Cell.Model -> Html
-- view address model =
--   Cell.view (Signal.forwardTo address Click) model
