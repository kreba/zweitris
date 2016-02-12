module Main where

import CellCollection
import Board
import MovingPiece
import Player
import KeyBindings

import Debug
import Effects
import Html exposing (..)
import StartApp
import Time


main : Signal Html
main =
  app.html


app : StartApp.App Model
app =
  StartApp.start
    { init = ( initialModel , Effects.none )
    , update = update
    , view = view
    , inputs = consumeKeypresses ++ continuousFalling
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
  { board = Board.init { w = 32 , h = 10 }
  , mpLeft = MovingPiece.init [ (1,3),(2,3),(2,4),(2,5) ] Player.Left
  , mpRight = MovingPiece.init [ (31,4),(32,3),(32,4),(32,5) ] Player.Right
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
  [ consumeKeyDown   "LeftPlayerTurn"    (RelayToMPLeft CellCollection.TurnCW)
  , consumeKeyFPS  6 "LeftPlayerGoUp"    (RelayToMPLeft (CellCollection.MoveBy ( 0 , -1 )))
  , consumeKeyFPS  6 "LeftPlayerGoDown"  (RelayToMPLeft (CellCollection.MoveBy ( 0 ,  1 )))
  , consumeKeyFPS 18 "LeftPlayerFall"    (RelayToMPLeft (CellCollection.MoveBy ( 1 ,  0 )))
  , consumeKeyDown   "RightPlayerTurn"   (RelayToMPRight CellCollection.TurnCW)
  , consumeKeyFPS  6 "RightPlayerGoUp"   (RelayToMPRight (CellCollection.MoveBy (  0 , -1 )))
  , consumeKeyFPS  6 "RightPlayerGoDown" (RelayToMPRight (CellCollection.MoveBy (  0 ,  1 )))
  , consumeKeyFPS 18 "RightPlayerFall"   (RelayToMPRight (CellCollection.MoveBy ( -1 ,  0 )))
  , consumeKeyDown   "TogglePause"       TogglePause
  ]

consumeKeyDown : String -> Action -> (Signal Action)
consumeKeyDown actionKey action =
    Signal.filterMap (onlyOnKeyDown action) Noop (KeyBindings.signalFor actionKey)

consumeKeyFPS : Int -> String -> Action -> (Signal Action)
consumeKeyFPS fps actionKey action =
    Signal.filterMap (onlyOnKeyDown action) Noop (Signal.sampleOn (Time.fps fps) (KeyBindings.signalFor actionKey))

onlyOnKeyDown : Action -> Bool -> (Maybe Action)
onlyOnKeyDown action down = if down then Maybe.Just action else Maybe.Nothing


continuousFalling : List (Signal Action)
continuousFalling =
  [ Signal.map (always <| RelayToMPLeft  <| CellCollection.MoveBy (  1 , 0 )) (Time.every Time.second)
  , Signal.map (always <| RelayToMPRight <| CellCollection.MoveBy ( -1 , 0 )) (Time.every Time.second)
  ]


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
