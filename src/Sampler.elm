module Sampler where

import Char
import Keyboard

import Html exposing (..)
import Time exposing (Time)


main : Signal Html
main =
  let
    sampler = Time.fps 20 -- Granularity: Check keyboard input every 50 ms. Make this slower to see what happens.
    s1 = Keyboard.isDown (Char.toCode 'N') |> triggerEvery (2000, sampler) "Every 2000 ms"
    s2 = Keyboard.isDown (Char.toCode 'M') |> triggerEvery ( 750, sampler) "Every 750 ms"
  in
    Signal.map
      (always (text "The debugger will show you how the sampler invokes actions while keys are being pressed. Try N and M."))
      (Signal.merge s1 s2)


triggerOnce : a -> Signal Bool -> Signal (Maybe a)
triggerOnce action actionActive =
  actionActive
    |> Signal.map (\becameActive -> if becameActive then Just action else Nothing)


triggerEvery : (Time, Signal Time) -> a -> Signal Bool -> Signal (Maybe a)
triggerEvery (interval, sampler) action actionActive =
  Signal.map2 (,) (Signal.sampleOn sampler actionActive) sampler
    |> Signal.foldp (every interval action) (Nothing, Inactive)
    |> Signal.map fst
    |> Signal.dropRepeats


type Timer = Inactive | Keeping Time
every : Time -> a -> (Bool, Time) -> (Maybe a, Timer) -> (Maybe a, Timer)
every interval action (becameActive, deltaT) (_, timer) =
  case (becameActive, timer) of

    (False, _) ->
      let d = Debug.watch (toString action) "Timer is inactive" in
      (Nothing, Inactive)

    (True, Inactive) ->
      let d = Debug.watch (toString action) "First invocation; initialize timer starting now" in
      (Just action, Keeping 0)

    (True, Keeping memoT) ->
      if deltaT + memoT > interval then
        let d = Debug.watch (toString action) "Next invocation; continue keeping track of time" in
        (Just action, Keeping (deltaT + memoT - interval))
      else
        let d = Debug.watch (toString action) ((toString (deltaT + memoT)) ++ " ms since last invocation") in
        (Nothing, Keeping (deltaT + memoT))
