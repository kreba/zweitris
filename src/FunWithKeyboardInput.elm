module FunWithKeyboardInput exposing (..)

import Player exposing (Player)
import Html exposing (Html, div, button, text)
import Html.Events exposing (onClick)
import Cmd
import Task
import Keyboard


type alias Model =
    Int


type Msg
    = Increment Int
    | Decrement Int
    | NoOp
    | Set Int


main : Program flags
main =
    Html.program
        { init = init 0
        , view = view
        , update = update
        , subscriptions =
            [ Sub.map (playerMsg Left) Keyboard.wasd
            , Sub.map (playerMsg Right) Keyboard.arrows
            , Sub.map resetMsg Keyboard.space
            ]
        }


init : Int -> ( Model, Cmd Msg )
init i =
    ( i, Cmd.none )


view : a -> Html Msg
view model =
    div []
        [ button [ onClick (Increment 1) ] [ text "+" ]
        , div [] [ text (toString model) ]
        , button [ onClick (Decrement 1) ] [ text "-" ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg oldModel =
    let
        newModel =
            case msg of
                NoOp ->
                    oldModel

                Increment int ->
                    oldModel + int

                Decrement int ->
                    oldModel - int

                Set int ->
                    int
    in
        ( newModel, Cmd.none )


playerMsg : Player -> { x : Int, y : Int } -> Msg
playerMsg player { x, y } =
    let
        times =
            case player of
                Left ->
                    10

                Right ->
                    1
    in
        case ( x, y ) of
            ( _, 1 ) ->
                Increment times

            ( _, -1 ) ->
                Decrement times

            ( _, _ ) ->
                NoOp


resetMsg : Bool -> Msg
resetMsg spaceDown =
    if spaceDown then
        Set 88
    else
        Set 0
