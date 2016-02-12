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
import Time


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
  | TogglePause
  | Reset
  | Noop


update : Action -> Model -> ( Model , Effects.Effects Action )
update action oldModel =
  let
    d1 = Debug.watchSummary "Main.update" toString action
    newModel = case action of

      RelayToBoard boardAction ->
        { oldModel | board = Board.update boardAction oldModel.board }

      RelayToMPLeft pieceAction ->
        { oldModel | mpLeft = MovingPiece.update pieceAction oldModel.mpLeft }

      RelayToMPRight pieceAction ->
        { oldModel | mpRight = MovingPiece.update pieceAction oldModel.mpRight }

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
  [ consumeKeyDown   "LeftPlayerTurn"    Noop
  , consumeKeyFPS  6 "LeftPlayerGoUp"    (RelayToMPLeft (CellCollection.MoveBy ( 0 , -1 )))
  , consumeKeyFPS  6 "LeftPlayerGoDown"  (RelayToMPLeft (CellCollection.MoveBy ( 0 ,  1 )))
  , consumeKeyFPS 18 "LeftPlayerFall"    (RelayToMPLeft (CellCollection.MoveBy ( 1 ,  0 )))
  , consumeKeyDown   "RightPlayerTurn"   Noop
  , consumeKeyFPS  6 "RightPlayerGoUp"   (RelayToMPRight (CellCollection.MoveBy (  0 , -1 )))
  , consumeKeyFPS  6 "RightPlayerGoDown" (RelayToMPRight (CellCollection.MoveBy (  0 ,  1 )))
  , consumeKeyFPS 18 "RightPlayerFall"   (RelayToMPRight (CellCollection.MoveBy ( -1 ,  0 )))
  , consumeKeyDown   "TogglePause"       TogglePause
  ]

consumeKeyDown : String -> Action -> (Signal Action)
consumeKeyDown actionKey action =
    Signal.filterMap (onlyOnKeyDown action) Noop (KeyBindings.signalFor actionKey)

{-| We should be able to implement this in a better way, i.e. always start a timer when a key is pressed.
    With the current implementation the repetition seems arbitrary at times.
    See http://www.stackoverflow.com/a/28737486/1212000
-}
consumeKeyFPS : Int -> String -> Action -> (Signal Action)
consumeKeyFPS fps actionKey action =
    Signal.filterMap (onlyOnKeyDown action) Noop (Signal.map2 always (KeyBindings.signalFor actionKey) (Time.fps fps))

onlyOnKeyDown : Action -> Bool -> (Maybe Action)
onlyOnKeyDown action down = if down then Maybe.Just action else Maybe.Nothing


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
