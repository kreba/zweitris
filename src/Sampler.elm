module Sampler exposing (..)

import Char
import Keyboard
import Html exposing (..)
import Time exposing (Time)


main : Sub Html
main =
    let
        sampler =
            Time.fps 20

        -- Granularity: Check keyboard input every 50 ms. Make this slower to see what happens.
        s1 =
            Keyboard.isDown (Char.toCode 'N') |> triggerEvery ( 2000, sampler ) "Every 2000 ms"

        s2 =
            Keyboard.isDown (Char.toCode 'M') |> triggerEvery ( 750, sampler ) "Every 750 ms"
    in
        Sub.map
            (always (text "The debugger will show you how the sampler sends messages while keys are being pressed. Try N and M."))
            (Sub.merge s1 s2)


triggerOnce : a -> Sub Bool -> Sub (Maybe a)
triggerOnce msg msgActive =
    msgActive
        |> Sub.map
            (\becameActive ->
                if becameActive then
                    Just msg
                else
                    Nothing
            )


triggerEvery : ( Time, Sub Time ) -> a -> Sub Bool -> Sub (Maybe a)
triggerEvery ( interval, sampler ) msg msgActive =
    Sub.map2 (,) (Sub.sampleOn sampler msgActive) sampler
        |> Sub.foldp (every interval msg) ( Nothing, Inactive )
        |> Sub.map Tuple.first
        |> Sub.dropRepeats


type Timer
    = Inactive
    | Keeping Time


every : Time -> a -> ( Bool, Time ) -> ( Maybe a, Timer ) -> ( Maybe a, Timer )
every interval msg ( becameActive, deltaT ) ( _, timer ) =
    case ( becameActive, timer ) of
        ( False, _ ) ->
            let
                d =
                    Debug.log (toString msg) "Timer is inactive"
            in
                ( Nothing, Inactive )

        ( True, Inactive ) ->
            let
                d =
                    Debug.log (toString msg) "First invocation; initialize timer starting now"
            in
                ( Just msg, Keeping 0 )

        ( True, Keeping memoT ) ->
            if deltaT + memoT > interval then
                let
                    d =
                        Debug.log (toString msg) "Next invocation; continue keeping track of time"
                in
                    ( Just msg, Keeping (deltaT + memoT - interval) )
            else
                let
                    d =
                        Debug.log (toString msg) ((toString (deltaT + memoT)) ++ " ms since last invocation")
                in
                    ( Nothing, Keeping (deltaT + memoT) )
