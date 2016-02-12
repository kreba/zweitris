module Main where

import CellCollection
import Board
import MovingPiece
import Player
import KeyBindings

import Debug
import Effects
import Html exposing (..)
import StartApp exposing (start)


main : Signal Html
main =
  app.html


app : StartApp.App Model
app =
  start
    { init = ( initialModel , Effects.none )
    , update = update
    , view = view
    , inputs = consumeKeypresses
    }


-- MODEL

type alias Model =
  { board : Board.Model
  , mpLeft : MovingPiece.Model
  , mpRight : MovingPiece.Model
  , paused : Bool
  }

initialModel : Model
initialModel =
  { board = Board.init { w = 20 , h = 8 }
  , mpLeft = MovingPiece.init [ (1,3),(2,3),(2,4),(2,5) ] Player.Left
  , mpRight = MovingPiece.init [ (19,4),(20,3),(20,4),(20,5) ] Player.Right
  , paused = False
  }


-- UPDATE

type Action
  = RelayToBoard CellCollection.Action
  | RelayToMPLeft CellCollection.Action
  | RelayToMPRight CellCollection.Action
  | LeftPlayerGoUp
  | LeftPlayerGoDown
  | TogglePause
  | Reset
  | Noop


update : Action -> Model -> ( Model , Effects.Effects Action )
update action oldModel =
  let
    d1 = Debug.watchSummary "update with action" toString action
    newModel = case action of

      RelayToBoard boardAction ->
        { oldModel | board = Board.update boardAction oldModel.board }

      RelayToMPLeft pieceAction ->
        { oldModel | mpLeft = MovingPiece.update pieceAction oldModel.mpLeft }

      RelayToMPRight pieceAction ->
        { oldModel | mpRight = MovingPiece.update pieceAction oldModel.mpRight }

      LeftPlayerGoUp ->
        { oldModel | mpLeft = MovingPiece.moveUp oldModel.mpLeft }

      LeftPlayerGoDown ->
        { oldModel | mpLeft = MovingPiece.moveDown oldModel.mpLeft }

      TogglePause ->
        { oldModel | paused = not oldModel.paused }

      Reset ->
        initialModel

      Noop ->
        oldModel

  in
    ( newModel, Effects.none )


consumeKeypresses : List (Signal Action)
consumeKeypresses =
  [ consume "LeftPlayerTurn"    Noop
  , consume "LeftPlayerGoUp"    LeftPlayerGoUp
  , consume "LeftPlayerGoDown"  LeftPlayerGoDown
  , consume "LeftPlayerFall"    Noop
  , consume "RightPlayerTurn"   Noop
  , consume "RightPlayerGoUp"   Noop
  , consume "RightPlayerGoDown" Noop
  , consume "RightPlayerFall"   Noop
  , consume "TogglePause"       TogglePause
  ]

consume : String -> Action -> (Signal Action)
consume actionKey action =
  let
    onlyOnKeyDown : Bool -> (Maybe Action)
    onlyOnKeyDown bool = if bool then Maybe.Just action else Maybe.Nothing
  in
    Signal.filterMap onlyOnKeyDown Noop (KeyBindings.signalFor actionKey)

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  let
    info = text (if model.paused then "PAUSED" else "RUNNING")
    board = Board.view (Signal.forwardTo address RelayToBoard) model.board
    mpLeft = MovingPiece.view (Signal.forwardTo address RelayToMPLeft) model.mpLeft
    mpRight = MovingPiece.view (Signal.forwardTo address RelayToMPRight) model.mpRight
  in
    div [] [ info , board , mpLeft , mpRight ]
