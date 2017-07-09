module MainUpdate exposing (update)

import MainModel exposing (..)
import MainMessages exposing (Msg(..))
import Tetrimino
import Time exposing (Time)
import Board
import CellCollection
import Player exposing (Player)
import Random
import Task exposing (Task)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg oldModel =
    let
        newModel =
            case msg of
                RelayToBoard boardMsg ->
                    { oldModel | board = Board.update boardMsg oldModel.board }

                Keypress key isDown ->
                    consumeKeyEvent ( key, isDown ) oldModel

                RelayToMP player tetriminoMsg ->
                    let
                        newTetrimino =
                            Tetrimino.update tetriminoMsg (getTetrimino player oldModel)
                    in
                        if acceptable newTetrimino oldModel then
                            setTetrimino player newTetrimino oldModel
                        else
                            oldModel

                TogglePause ->
                    { oldModel | paused = not oldModel.paused }

                Reset ->
                    MainModel.initial

                Tick timediff ->
                    animate (min timediff 25) oldModel

                Noop ->
                    oldModel
    in
        ( newModel, Cmd.none )


consumeKeyEvent : ( KeyDown, Bool ) -> Model -> Model
consumeKeyEvent ( key, isDown ) model =
    case key of
        PlayerLeftRotate ->
            { model | keyLeftRotate = isDown }
                |> startstopRotateLeft

        PlayerLeftMoveUp ->
            { model | keyLeftMoveUp = isDown }
                |> startstopMoveLeft

        PlayerLeftMoveDown ->
            { model | keyLeftMoveDown = isDown }
                |> startstopMoveLeft

        PlayerLeftAccelerate ->
            { model | keyLeftAccelerate = isDown }
                |> resetDropAnimationLeft

        PlayerRightRotate ->
            { model | keyRightRotate = isDown }
                |> startstopRotateRight

        PlayerRightMoveUp ->
            { model | keyRightMoveUp = isDown }
                |> startstopMoveRight

        PlayerRightMoveDown ->
            { model | keyRightMoveDown = isDown }
                |> startstopMoveRight

        PlayerRightAccelerate ->
            { model | keyRightAccelerate = isDown }
                |> resetDropAnimationRight


startstopRotateLeft : Model -> Model
startstopRotateLeft model =
    if model.keyLeftRotate then
        if model.animLeftRotating == Nothing then
            { model | animLeftRotating = Just { millisTillNextStep = 0 } }
        else
            model
    else
        { model | animLeftRotating = Nothing }


startstopRotateRight : Model -> Model
startstopRotateRight model =
    if model.keyRightRotate then
        if model.animRightRotating == Nothing then
            { model | animRightRotating = Just { millisTillNextStep = 0 } }
        else
            model
    else
        { model | animRightRotating = Nothing }


startstopMoveLeft : Model -> Model
startstopMoveLeft model =
    if model.keyLeftMoveUp /= model.keyLeftMoveDown then
        if model.animLeftMoving == Nothing then
            { model | animLeftMoving = Just { millisTillNextStep = 0 } }
        else
            model
    else
        { model | animLeftMoving = Nothing }


startstopMoveRight : Model -> Model
startstopMoveRight model =
    if model.keyRightMoveUp /= model.keyRightMoveDown then
        if model.animRightMoving == Nothing then
            { model | animRightMoving = Just { millisTillNextStep = 0 } }
        else
            model
    else
        { model | animRightMoving = Nothing }


animate : Time -> Model -> Model
animate elapsed model =
    model
        |> moveTetriminoLeft elapsed
        |> moveTetriminoRight elapsed
        |> rotateTetriminoLeft elapsed
        |> rotateTetriminoRight elapsed
        |> dropTetriminoLeft elapsed
        |> dropTetriminoRight elapsed
        |> checkEndGame



moveTetriminoLeft : Time -> Model -> Model
moveTetriminoLeft elapsed model =
    case model.animLeftMoving of
        Just anim ->
            let
                ( anim_, shouldStep ) = advanceAnimation 200 elapsed anim
                model_ = { model | animLeftMoving = Just anim_ }
            in
                if shouldStep then
                    moveTetrimino_ Player.Left model_
                else
                    model_
        Nothing ->
            model


moveTetriminoRight : Time -> Model -> Model
moveTetriminoRight elapsed model =
    case model.animRightMoving of
        Just anim ->
            let
                ( anim_, shouldStep ) = advanceAnimation 200 elapsed anim
                model_ = { model | animRightMoving = Just anim_ }
            in
                if shouldStep then
                    moveTetrimino_ Player.Right model_
                else
                    model_
        Nothing ->
            model


moveTetrimino_ player model =
    model
    |> update (RelayToMP player (CellCollection.Move (movementDirection player model)))
    |> Tuple.first


rotateTetriminoLeft : Time -> Model -> Model
rotateTetriminoLeft elapsed model =
    case model.animLeftRotating of
        Just anim ->
            let
                ( anim_, shouldStep ) = advanceAnimation 300 elapsed anim
                model_ = { model | animLeftRotating = Just anim_ }
            in
                if shouldStep then
                    rotateTetrimino_ Player.Left model_
                else
                    model_
        Nothing ->
            model


rotateTetriminoRight : Time -> Model -> Model
rotateTetriminoRight elapsed model =
    case model.animRightRotating of
        Just anim ->
            let
                ( anim_, shouldStep ) = advanceAnimation 300 elapsed anim
                model_ = { model | animRightRotating = Just anim_ }
            in
                if shouldStep then
                    rotateTetrimino_ Player.Right model_
                else
                    model_
        Nothing ->
            model


rotateTetrimino_ player model =
    model
    |> update (RelayToMP player CellCollection.Rotate)
    |> Tuple.first


dropTetriminoLeft : Time -> Model -> Model
dropTetriminoLeft elapsed model =
    let
        anim = model.animLeftDropping
        interval = if model.keyLeftAccelerate then
                       25
                   else
                       300
        ( anim_, shouldStep ) = advanceAnimation interval elapsed anim
        model_ = { model | animLeftDropping = anim_ }
    in
        if shouldStep then
            dropTetrimino_ Player.Left model_
        else
            model_


dropTetriminoRight : Time -> Model -> Model
dropTetriminoRight elapsed model =
    let
        anim = model.animRightDropping
        interval = if model.keyRightAccelerate then
                       25
                   else
                       300
        ( anim_, shouldStep ) = advanceAnimation interval elapsed anim
        model_ = { model | animRightDropping = anim_ }
    in
        if shouldStep then
            dropTetrimino_ Player.Right model_
        else
            model_


resetDropAnimationLeft : Model -> Model
resetDropAnimationLeft model =
    { model | animLeftDropping = { millisTillNextStep = 0 } }


resetDropAnimationRight : Model -> Model
resetDropAnimationRight model =
    { model | animRightDropping = { millisTillNextStep = 0 } }


dropTetrimino_ : Player -> Model -> Model
dropTetrimino_ player model =
    let
        oldTetrimino =
            getTetrimino player model

        updatedTetrimino =
            Tetrimino.update (CellCollection.Move (dropDirection player model)) oldTetrimino

    in
        if acceptable updatedTetrimino model then
            setTetrimino player updatedTetrimino model
        else
            mergeTetriminoAndScore player model

advanceAnimation : Time -> Time -> { millisTillNextStep : Time } -> ({ millisTillNextStep : Time }, Bool)
advanceAnimation interval elapsed anim =
    let
        remaining = anim.millisTillNextStep - elapsed
    in
        if remaining <= 0 then
            ( {anim | millisTillNextStep = remaining + interval} , True )
        else
            ( {anim | millisTillNextStep = remaining}, False )


movementDirection player model =
    let
       (keyUp, keyDown) = case player of
            Player.Left ->
                ( model.keyLeftMoveUp, model.keyLeftMoveDown )

            Player.Right ->
                ( model.keyRightMoveUp, model.keyRightMoveDown )
    in
        case (keyUp, keyDown) of
            ( True, False ) ->
                Board.directionUp

            ( False, True ) ->
                Board.directionDown

            ( _, _ ) ->
                ( 0,0 )


dropDirection player model =
   case player of
        Player.Left ->
            Board.directionRight

        Player.Right ->
            Board.directionLeft


mergeTetriminoAndScore player model =
    let
        oldTetrimino =
            getTetrimino player model

        ( newTetrimino, newSeed ) =
            Tetrimino.init player model.seed

        withNewTetrimino =
            setTetrimino player newTetrimino model
    in
        if acceptable newTetrimino model then
            { withNewTetrimino
                | board =
                    Board.update (CellCollection.MergeFrom oldTetrimino.cells) model.board
                        |> Board.score player oldTetrimino.cells
                , seed = newSeed
            }
        else
            MainModel.initial

getTetrimino player model =
    case player of
        Player.Left ->
            model.tetriminoLeft

        Player.Right ->
            model.tetriminoRight

setTetrimino player tetrimino model =
    case player of
        Player.Left ->
            { model | tetriminoLeft = tetrimino }

        Player.Right ->
            { model | tetriminoRight = tetrimino }

acceptable tetrimino model =
    let
        withinBoard tetriminoCell =
            let
                ( x, y ) =
                    tetriminoCell.pos

                ( w, h ) =
                    model.board.size
            in
                (0 < x && x <= w) && (0 < y && y <= h)

        noCollision tetriminoCell =
            List.all (\boardCell -> boardCell.pos /= tetriminoCell.pos || boardCell.owner /= tetriminoCell.owner) model.board.cells
    in
        List.all withinBoard tetrimino.cells
            && List.all noCollision tetrimino.cells

checkEndGame : Model -> Model
checkEndGame model =
    model


