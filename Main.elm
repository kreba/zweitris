import Cell exposing (update, view)
import StartApp.Simple exposing (start)
import Html

main: Signal Html.Html
main =
  start
    { model = "lightgreen"
    , update = update
    , view = view
    }
