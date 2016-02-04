module Body (Model, init, update, view) where

import Debug
import Cell exposing (Action)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)



-- MODEL

type alias Model = List Cell.Model


init : (Int, Int) -> Model
init (x, y) = [Cell.init "lightgreen"]


-- UPDATE

update : Action -> Model -> Model
update action model = model


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div [] 
    (List.map (\cellModel -> (Cell.view address cellModel)) model) ++ div [] []

cellStyle : Attribute
cellStyle =
  style
    [ ("background-color", "lightblue")
    , ("width", "50px") 
    , ("height", "50px") 
    ]