module Main where

import CellCollection
import Board
import MovingPiece
import Player
import KeyBindings

import Effects
import Html exposing (..)
import Random
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

boardWidth : Int
boardWidth = 32

boardHight : Int
boardHight = 10

-- MODEL

type alias Model =
  { board : Board.Model
  , mpLeft : MovingPiece.Model
  , mpRight : MovingPiece.Model
  , paused : Bool
  , seed : Random.Seed
  }

initialModel : Model
initialModel =
  let
    seed1 = Random.initialSeed 22536475869
    (mpLeft, seed2) = MovingPiece.init Player.Left seed1
    (mpRight, seed) = MovingPiece.init Player.Right seed2
  in
    { board   = Board.init { w = boardWidth , h = boardHight }
    , mpLeft  = mpLeft
    , mpRight = mpRight
    , paused  = False
    , seed    = seed
    }


-- UPDATE

type Action
  = RelayToBoard CellCollection.Action
  | RelayToMP Player.Player CellCollection.Action
  | ContinuousFall Player.Player
  | TogglePause
  | Reset
  | Noop


update : Action -> Model -> ( Model , Effects.Effects Action )
update action oldModel =
  let
    getPiece player = case player of
      Player.Left  -> oldModel.mpLeft
      Player.Right -> oldModel.mpRight

    setPiece player piece = case player of
      Player.Left  -> { oldModel | mpLeft  = piece }
      Player.Right -> { oldModel | mpRight = piece }

    withinBoard pieceCell =
      let (x,y) = pieceCell.pos
          (w,h) = oldModel.board.size
      in  (0 < x && x <= w) && (0 < y && y <= h)

    noCollision pieceCell =
      List.all (\boardCell -> boardCell.pos /= pieceCell.pos || boardCell.owner /= pieceCell.owner) oldModel.board.cells

    acceptable piece =
      List.all withinBoard piece.cells &&
      List.all noCollision piece.cells

    newModel = case action of

      RelayToBoard boardAction ->
        { oldModel | board = Board.update boardAction oldModel.board }

      ContinuousFall player ->
        let
          oldPiece = getPiece player
          pieceAction =
            case player of
              Player.Left  -> CellCollection.MoveBy Board.right
              Player.Right -> CellCollection.MoveBy Board.left
          updatedPiece = MovingPiece.update pieceAction oldPiece
        in
          if acceptable updatedPiece then
            setPiece player updatedPiece
          else
            let
              (newPiece, newSeed) = MovingPiece.init player oldModel.seed
              nom = setPiece player newPiece
            in
              { nom
              | board = Board.update (CellCollection.MergeFrom oldPiece.cells) oldModel.board
              , seed = newSeed
              }

      RelayToMP player pieceAction ->
        let
          newPiece = MovingPiece.update pieceAction (getPiece player)
        in
          if acceptable newPiece then
            setPiece player newPiece
          else
            oldModel

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
  [ Time.every Time.second |> Signal.map (\_ -> ContinuousFall Player.Left)
  , Time.every Time.second |> Signal.map (\_ -> ContinuousFall Player.Right)
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
