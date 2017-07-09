module Main exposing (..)

import AnimationFrame
import Char
import MainModel exposing (Model, KeyDown(..))
import MainMessages exposing (Msg(..))
import MainUpdate
import MainView
import Keyboard
import Html exposing (Html, button, div, text)


main : Program Never Model Msg
main =
    Html.program
        { init = ( MainModel.initial, Cmd.none )
        , update = MainUpdate.update
        , view = MainView.view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Tick
        , Keyboard.downs (key True)
        , Keyboard.ups (key False)
        ]


key : Bool -> Char.KeyCode -> Msg
key isDown keycode =
    case keycode of
        68 ->
            -- D
            Keypress PlayerLeftAccelerate isDown

        83 ->
            -- S
            Keypress PlayerLeftMoveDown isDown

        87 ->
            -- W
            Keypress PlayerLeftMoveUp isDown

        65 ->
            -- A
            Keypress PlayerLeftRotate isDown

        37 ->
            -- Left Arrow
            Keypress PlayerRightAccelerate isDown

        40 ->
            -- Down Arrow
            Keypress PlayerRightMoveDown isDown

        38 ->
            -- Up Arrow
            Keypress PlayerRightMoveUp isDown

        39 ->
            -- Right Arrow
            Keypress PlayerRightRotate isDown

        32 ->
            -- Space
            if isDown then
                TogglePause
            else
                Noop

        _ ->
            Noop
