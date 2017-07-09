module MainView exposing (view)

import Html exposing (div, Html, text, button)
import Collage
import Element
import Color exposing (Color)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onMouseDown, onMouseUp, on)
import Markdown
import MainModel exposing (Model)
import MainMessages exposing (Msg(..))
import Board
import Player
import Tetrimino
import Json.Decode as Json


view : Model -> Html Msg
view model =
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

        tetriminoLeft =
            Html.map (RelayToMP Player.Left) (Tetrimino.view model.tetriminoLeft)

        tetriminoRight =
            Html.map (RelayToMP Player.Right) (Tetrimino.view model.tetriminoRight)
    in
        div [] [ info, board, tetriminoLeft, tetriminoRight ]



--(=>) : a -> b -> ( a, b )
--(=>) =
--    (,)
--
--
--view : Model -> Html Msg
--view model =
--    div
--        [ style [ "padding" => "30px 0" ]
--        , onTouchEnd Msg.UnlockButtons
--        , onMouseUp Msg.UnlockButtons
--        ]
--        [ div
--            [ style
--                [ "height" => "680px"
--                , "margin" => "auto"
--                , "position" => "relative"
--                , "width" => "480px"
--                ]
--            ]
--            [ renderWell model
--            , renderControls
--            , renderPanel model
--            , renderInfo model.state
--            ]
--        ]
