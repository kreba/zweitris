import Cell exposing (update, view)
import StartApp.Simple exposing (start)


main =
  start
    { model = "blue"
    , update = update
    , view = view
    }
