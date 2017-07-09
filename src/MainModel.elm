module MainModel exposing (Model, State(..), AnimationState, KeyDown(..), initial)

import Board
import Player
import Tetrimino
import Random
import Color exposing (Color)
import Time exposing (Time)


type State
    = Paused
    | Playing
    | Stopped


type KeyDown
    = PlayerLeftRotate
    | PlayerLeftMoveUp
    | PlayerLeftMoveDown
    | PlayerLeftAccelerate
    | PlayerRightRotate
    | PlayerRightMoveUp
    | PlayerRightMoveDown
    | PlayerRightAccelerate


type alias AnimationState =
    { millisTillNextStep : Time }


type alias Model =
    { board : Board.Model
    , tetriminoLeft : Tetrimino.Model
    , tetriminoRight : Tetrimino.Model
    , keyLeftRotate : Bool
    , keyLeftMoveUp : Bool
    , keyLeftMoveDown : Bool
    , keyLeftAccelerate : Bool
    , keyRightRotate : Bool
    , keyRightMoveUp : Bool
    , keyRightMoveDown : Bool
    , keyRightAccelerate : Bool
    , animLeftRotating : Maybe AnimationState
    , animRightRotating : Maybe AnimationState
    , animLeftMoving : Maybe AnimationState
    , animRightMoving : Maybe AnimationState
    , animLeftDropping : AnimationState
    , animRightDropping : AnimationState
    , paused : Bool
    , seed : Random.Seed
    }


initial : Model
initial =
    let
        seed1 =
            Random.initialSeed 22536475869

        ( tetriminoLeft, seed2 ) =
            Tetrimino.init Player.Left seed1

        ( tetriminoRight, seed ) =
            Tetrimino.init Player.Right seed2
    in
        { board = Board.init { w = 32, h = 10 }
        , tetriminoLeft = tetriminoLeft
        , tetriminoRight = tetriminoRight
        , keyLeftRotate = False
        , keyLeftMoveUp = False
        , keyLeftMoveDown = False
        , keyLeftAccelerate = False
        , keyRightRotate = False
        , keyRightMoveUp = False
        , keyRightMoveDown = False
        , keyRightAccelerate = False
        , animLeftRotating = Maybe.Nothing
        , animLeftMoving = Maybe.Nothing
        , animLeftDropping = { millisTillNextStep = 0 }
        , animRightRotating = Maybe.Nothing
        , animRightMoving = Maybe.Nothing
        , animRightDropping = { millisTillNextStep = 0 }
        , paused = False
        , seed = seed
        }
