module Cell exposing (..)

import Player exposing (Player)
import Html exposing (..)
import Html.Attributes exposing (id, class, style)


-- MODEL


type alias Position =
    ( Int, Int )


type alias Model =
    { pos : Position, owner : Player }


init : Position -> Player -> Model
init position player =
    { pos = position, owner = player }



-- UPDATE


type Msg
    = SetOwner Player
    | MoveBy Position


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetOwner player ->
            { model | owner = player }

        MoveBy ( x, y ) ->
            { model | pos = ( Tuple.first model.pos + x, Tuple.second model.pos + y ) }



-- VIEW


view : Model -> Html Msg
view model =
    let
        ( posX, posY ) =
            model.pos

        cellSize =
            45

        elementStyle =
            [ ( "background-color", Player.color model.owner )
            , ( "color", Player.color model.owner )
            , ( "box-sizing", "border-box" )
            , ( "width", toString cellSize ++ "px" )
            , ( "height", toString cellSize ++ "px" )
            , ( "padding", "3px" )
            , ( "border", "1px solid white" )
            , ( "position", "absolute" )
            , ( "left", toString (posX * cellSize) ++ "px" )
            , ( "top", toString (posY * cellSize) ++ "px" )
            , ( "font-size", "6px" )
            ]
    in
        div
            [ class "cell"
            , id ("cell-" ++ toString posX ++ "-" ++ toString posY)
            , style elementStyle
            ]
            [ text (toString posX ++ ", " ++ toString posY) ]
