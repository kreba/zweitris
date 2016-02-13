module Main where

import CellCollection
import Board
import MovingPiece
import Player
import KeyBindings

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
  | RelayToMP Player.Player CellCollection.Action
  | TogglePause
  | Reset
  | Noop


update : Action -> Model -> ( Model , Effects.Effects Action )
update action oldModel =
  let
    newModel = case action of

      RelayToBoard boardAction ->
        { oldModel | board = Board.update boardAction oldModel.board }

      RelayToMP player pieceAction ->
        let

          getPiece player = case player of
            Player.Left  -> oldModel.mpLeft
            Player.Right -> oldModel.mpRight

          setPiece player piece = case player of
            Player.Left  -> { oldModel | mpLeft  = piece }
            Player.Right -> { oldModel | mpRight = piece }

        in
          setPiece player <| MovingPiece.update pieceAction (getPiece player)

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
  let slowly = 6
      fast = 18
      onKeyDown action isDown = if isDown then Maybe.Just action else Maybe.Nothing
      consumeAs action = Signal.filterMap (onKeyDown action) Noop
      once = consumeAs
      repeat fps action = consumeAs action << Signal.sampleOn (Time.fps fps)
  in  [ KeyBindings.signalFor "LeftPlayerTurn"    |> once          (RelayToMP Player.Left CellCollection.TurnCW)
      , KeyBindings.signalFor "LeftPlayerGoUp"    |> repeat slowly (RelayToMP Player.Left (CellCollection.MoveBy Board.up))
      , KeyBindings.signalFor "LeftPlayerGoDown"  |> repeat slowly (RelayToMP Player.Left (CellCollection.MoveBy Board.down))
      , KeyBindings.signalFor "LeftPlayerFall"    |> repeat fast   (RelayToMP Player.Left (CellCollection.MoveBy Board.right))
      , KeyBindings.signalFor "RightPlayerTurn"   |> once          (RelayToMP Player.Right CellCollection.TurnCW)
      , KeyBindings.signalFor "RightPlayerGoUp"   |> repeat slowly (RelayToMP Player.Right (CellCollection.MoveBy Board.up))
      , KeyBindings.signalFor "RightPlayerGoDown" |> repeat slowly (RelayToMP Player.Right (CellCollection.MoveBy Board.down))
      , KeyBindings.signalFor "RightPlayerFall"   |> repeat fast   (RelayToMP Player.Right (CellCollection.MoveBy Board.left))
      , KeyBindings.signalFor "TogglePause"       |> once          TogglePause
      ]


continuousFalling : List (Signal Action)
continuousFalling =
  [ Time.every Time.second |> Signal.map (\_ -> RelayToMP Player.Left  (CellCollection.MoveBy (  1 , 0 )))
  , Time.every Time.second |> Signal.map (\_ -> RelayToMP Player.Right (CellCollection.MoveBy ( -1 , 0 )))
  ]


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  let
    info = text (if model.paused then "PAUSED" else "RUNNING")
    board = Board.view (Signal.forwardTo address RelayToBoard) model.board
    mpLeft = MovingPiece.view (Signal.forwardTo address (RelayToMP Player.Left)) model.mpLeft
    mpRight = MovingPiece.view (Signal.forwardTo address (RelayToMP Player.Right)) model.mpRight
  in
    div [] [ info , board , mpLeft , mpRight ]
