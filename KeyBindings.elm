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
  [ ( "leftPlayerTurn"    , Char.toCode 'A' )
  , ( "leftPlayerGoUp"    , Char.toCode 'Q' )
  , ( "leftPlayerGoDown"  , Char.toCode 'S' )
  , ( "leftPlayerFall"    , Char.toCode 'W' )
  , ( "rightPlayerTurn"   , Char.toCode 'L' )
  , ( "rightPlayerGoUp"   , Char.toCode 'P' )
  , ( "rightPlayerGoDown" , Char.toCode 'K' )
  , ( "rightPlayerFall"   , Char.toCode 'O' )
  , ( "pause"             , Char.toCode ' ' )
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
  [ codesFor [ "leftPlayerTurn" ]
  , codesFor [ "leftPlayerGoUp" , "leftPlayerGoDown" ]
  , codesFor [ "leftPlayerFall" ]
  , codesFor [ "rightPlayerTurn" ]
  , codesFor [ "rightPlayerGoUp" , "rightPlayerGoDown" ]
  , codesFor [ "rightPlayerFall" ]
  , codesFor [ "pause" ]
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
    d1 = Debug.watch "pressedKeys" (Set.foldr (++) "" (Set.map (String.fromChar << Char.fromCode) keyCodes))
    d2 = Debug.watch "acceptedKeys" (Set.foldr (++) "" (Set.map (String.fromChar << Char.fromCode) allAcceptedKeys))
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
