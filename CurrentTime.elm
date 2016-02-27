module CurrentTime where
import Task
import Time
{-| This task results in the current time. Whenever the task is performed, it
will look at the current time and give it to you.
-}
getCurrentTime : Task.Task x Time.Time
getCurrentTime =
  Native.CurrentTime.getCurrentTime
