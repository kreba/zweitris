module Cell (Model, init, Action, update, view) where

-- import Debug
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


-- MODEL
type alias Model = String


init : String -> Model
init color = color


-- UPDATE

type Action = Click

update : Action -> Model -> Model
update action model =
  case model of
    "lightgreen" -> "lightblue"
    "lightblue"  -> "lightgreen"
    _ -> "lightblue"



-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div [ cellStyle model, onClick address Click ] []

cellStyle : Model -> Attribute
cellStyle model =
  style
    [ ("background-color", model)
    , ("width", "50px")
    , ("height", "50px")
    ]
