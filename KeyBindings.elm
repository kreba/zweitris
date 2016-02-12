module KeyBindings where

import Char
import Dict exposing (Dict)
import Keyboard
import Set exposing (Set)
import String


keysDown : Signal (Set Char.KeyCode)
keysDown = Signal.map filter Keyboard.keysDown


{- Lists all the key strokes that can invoke actions in our app.
-}
all : Dict String Char.KeyCode
all = Dict.fromList
  [ ( "LeftPlayerTurn"    , Char.toCode 'A' )
  , ( "LeftPlayerGoUp"    , Char.toCode 'Q' )
  , ( "LeftPlayerGoDown"  , Char.toCode 'S' )
  , ( "LeftPlayerFall"    , Char.toCode 'W' )
  , ( "RightPlayerTurn"   , Char.toCode 'L' )
  , ( "RightPlayerGoUp"   , Char.toCode 'P' )
  , ( "RightPlayerGoDown" , Char.toCode 'K' )
  , ( "RightPlayerFall"   , Char.toCode 'O' )
  , ( "TogglePause"       , Char.toCode ' ' )
  ]


signalFor : String -> (Signal Bool)
signalFor actionKey =
  let
    keyCode = Maybe.withDefault 0 (codeFor actionKey)
  in
    Signal.dropRepeats (Signal.map (Set.member keyCode) keysDown)

{- Lists all key codes that are relevant to our app.
They are grouped in sets of mutually exclusive actions.
If multiple keys in a mutex set are pressed together, we will not invoke any of their respective actions.
-}
mutexSets : List (Set Char.KeyCode)
mutexSets =
  [ codesFor [ "LeftPlayerTurn" ]
  , codesFor [ "LeftPlayerGoUp" , "LeftPlayerGoDown" ]
  , codesFor [ "LeftPlayerFall" ]
  , codesFor [ "RightPlayerTurn" ]
  , codesFor [ "RightPlayerGoUp" , "RightPlayerGoDown" ]
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
    d2 = Debug.watch "accepted keys" (Set.foldr (++) "" (Set.map (String.fromChar << Char.fromCode) allAcceptedKeys))
    allAcceptedKeys = List.foldr accept Set.empty mutexSets
    accept mutexSet acceptedKeys =
      let
        matchingKeys = Set.intersect mutexSet keyCodes
      in
        if (Set.size matchingKeys) == 1 then
          Set.union matchingKeys acceptedKeys
        else
          acceptedKeys
  in
    allAcceptedKeys
