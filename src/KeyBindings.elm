module KeyBindings exposing (..)

import Char
import Dict exposing (Dict)
import Keyboard
import Set exposing (Set)
import String


keysDown : Sub (Set Char.KeyCode)
keysDown =
    Sub.map filter Keyboard.keysDown



{- Lists all the key strokes that can invoke msgs in our app. -}


all : Dict String Char.KeyCode
all =
    Dict.fromList
        [ ( "LeftPlayerTurn", Char.toCode 'A' )
        , ( "LeftPlayerGoUp", Char.toCode 'W' )
        , ( "LeftPlayerGoDown", Char.toCode 'S' )
        , ( "LeftPlayerFall", Char.toCode 'D' )
        , ( "RightPlayerTurn", 39 ) -- Right Arrow
        , ( "RightPlayerGoUp", 38 ) -- Up Arrow
        , ( "RightPlayerGoDown", 40 ) -- Down Arrow
        , ( "RightPlayerFall", 37 ) -- Left Arrow
        , ( "TogglePause", Char.toCode ' ' )
        ]


subFor : String -> Sub Bool
subFor msgKey =
    let
        keyCode =
            Maybe.withDefault 0 (codeFor msgKey)
    in
        Sub.dropRepeats (Sub.map (Set.member keyCode) keysDown)



{- Lists all key codes that are relevant to our app.
   They are grouped in sets of mutually exclusive msgs.
   If multiple keys in a mutex set are pressed together, we will not invoke any of their respective msgs.
-}


mutexSets : List (Set Char.KeyCode)
mutexSets =
    [ codesFor [ "LeftPlayerTurn" ]
    , codesFor [ "LeftPlayerGoUp", "LeftPlayerGoDown" ]
    , codesFor [ "LeftPlayerFall" ]
    , codesFor [ "RightPlayerTurn" ]
    , codesFor [ "RightPlayerGoUp", "RightPlayerGoDown" ]
    , codesFor [ "RightPlayerFall" ]
    , codesFor [ "TogglePause" ]
    ]


codesFor : List String -> Set Char.KeyCode
codesFor keys =
    List.filterMap codeFor keys |> Set.fromList


codeFor : String -> Maybe Char.KeyCode
codeFor key =
    Dict.get key all



{- Accepts only key codes that are relevant for our app.
   Rejects mutually exclusive keys when pressed together.
-}


filter : Set Char.KeyCode -> Set Char.KeyCode
filter keyCodes =
    let
        d2 =
            Debug.log "accepted keys" (Set.foldr (++) "" (Set.map (String.fromChar << Char.fromCode) allAcceptedKeys))

        allAcceptedKeys =
            List.foldr accept Set.empty mutexSets

        accept mutexSet acceptedKeys =
            let
                matchingKeys =
                    Set.intersect mutexSet keyCodes
            in
                if (Set.size matchingKeys) == 1 then
                    Set.union matchingKeys acceptedKeys
                else
                    acceptedKeys
    in
        allAcceptedKeys
