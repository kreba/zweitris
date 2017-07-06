module Main exposing (..)

import CellCollection
import Board
import MovingPiece
import Player
import KeyBindings
import Sampler
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Random
import Time


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


boardWidth : Int
boardWidth =
    32


boardHight : Int
boardHight =
    10



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
        seed1 =
            Random.initialSeed 22536475869

        ( mpLeft, seed2 ) =
            MovingPiece.init Player.Left seed1

        ( mpRight, seed ) =
            MovingPiece.init Player.Right seed2
    in
        { board = Board.init { w = boardWidth, h = boardHight }
        , mpLeft = mpLeft
        , mpRight = mpRight
        , paused = False
        , seed = seed
        }



-- UPDATE


type Msg
    = RelayToBoard CellCollection.Msg
    | RelayToMP Player.Player CellCollection.Msg
    | ContinuousFall Player.Player
    | TogglePause
    | Reset
    | Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg oldModel =
    let
        getPiece player =
            case player of
                Player.Left ->
                    oldModel.mpLeft

                Player.Right ->
                    oldModel.mpRight

        setPiece player piece =
            case player of
                Player.Left ->
                    { oldModel | mpLeft = piece }

                Player.Right ->
                    { oldModel | mpRight = piece }

        withinBoard pieceCell =
            let
                ( x, y ) =
                    pieceCell.pos

                ( w, h ) =
                    oldModel.board.size
            in
                (0 < x && x <= w) && (0 < y && y <= h)

        noCollision pieceCell =
            List.all (\boardCell -> boardCell.pos /= pieceCell.pos || boardCell.owner /= pieceCell.owner) oldModel.board.cells

        acceptable piece =
            List.all withinBoard piece.cells
                && List.all noCollision piece.cells

        mergePieceAndScore player =
            let
                oldPiece =
                    getPiece player

                ( newPiece, newSeed ) =
                    MovingPiece.init player oldModel.seed

                withNewPiece =
                    setPiece player newPiece
            in
                if acceptable newPiece then
                    { withNewPiece
                        | board =
                            Board.update (CellCollection.MergeFrom oldPiece.cells) oldModel.board
                                |> Board.score player oldPiece.cells
                        , seed = newSeed
                    }
                else
                    initialModel

        -- reset
        newModel =
            case msg of
                RelayToBoard boardMsg ->
                    { oldModel | board = Board.update boardMsg oldModel.board }

                ContinuousFall player ->
                    let
                        oldPiece =
                            getPiece player

                        pieceMsg =
                            case player of
                                Player.Left ->
                                    CellCollection.MoveBy Board.right

                                Player.Right ->
                                    CellCollection.MoveBy Board.left

                        updatedPiece =
                            MovingPiece.update pieceMsg oldPiece
                    in
                        if acceptable updatedPiece then
                            setPiece player updatedPiece
                        else
                            mergePieceAndScore player

                RelayToMP player pieceMsg ->
                    let
                        newPiece =
                            MovingPiece.update pieceMsg (getPiece player)
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
        ( newModel, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions =
    Sub.batch consumeKeypresses ++ continuousFalling


consumeKeypresses : List (Sub Msg)
consumeKeypresses =
    let
        slowly =
            200

        fast =
            60

        granularity =
            Time.every Time.second * 1 / 50

        once msg signal =
            Sampler.triggerOnce msg signal |> invocations

        repeat interval msg signal =
            Sampler.triggerEvery ( interval, granularity ) msg signal |> invocations

        invocations =
            Sub.filterMap identity Noop
    in
        [ KeyBindings.subFor "LeftPlayerTurn" |> once (RelayToMP Player.Left CellCollection.TurnCW)
        , KeyBindings.subFor "LeftPlayerGoUp" |> repeat slowly (RelayToMP Player.Left (CellCollection.MoveBy Board.up))
        , KeyBindings.subFor "LeftPlayerGoDown" |> repeat slowly (RelayToMP Player.Left (CellCollection.MoveBy Board.down))
        , KeyBindings.subFor "LeftPlayerFall" |> repeat fast (RelayToMP Player.Left (CellCollection.MoveBy Board.right))
        , KeyBindings.subFor "RightPlayerTurn" |> once (RelayToMP Player.Right CellCollection.TurnCW)
        , KeyBindings.subFor "RightPlayerGoUp" |> repeat slowly (RelayToMP Player.Right (CellCollection.MoveBy Board.up))
        , KeyBindings.subFor "RightPlayerGoDown" |> repeat slowly (RelayToMP Player.Right (CellCollection.MoveBy Board.down))
        , KeyBindings.subFor "RightPlayerFall" |> repeat fast (RelayToMP Player.Right (CellCollection.MoveBy Board.left))
        , KeyBindings.subFor "TogglePause" |> once TogglePause
        ]


continuousFalling : List (Sub Msg)
continuousFalling =
    [ Time.every Time.second (\_ -> ContinuousFall Player.Left)
    , Time.every Time.second (\_ -> ContinuousFall Player.Right)
    ]



-- VIEW


view : Model -> Html Msg
view address model =
    let
        info =
            text
                (if model.paused then
                    "PAUSED"
                 else
                    "RUNNING"
                )

        board =
            Html.map RelayToBoard (Board.view model.board)

        mpLeft =
            Html.map (RelayToMP Player.Left) (MovingPiece.view model.mpLeft)

        mpRight =
            Html.map (RelayToMP Player.Right) (MovingPiece.view model.mpRight)
    in
        div [] [ info, board, mpLeft, mpRight ]
