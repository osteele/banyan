module CmdExtras exposing (..)

import Process
import Task
import Time exposing (Time)


{-| Construct a message from a command.
-}
message : msg -> Cmd msg
message =
    Task.perform identity << Task.succeed


{-| Construct a message that is executed after a delay.
-}
sendDelayed : Time -> Float -> msg -> Cmd msg
sendDelayed time unit msg =
    Process.sleep (time * unit)
        |> Task.andThen (always <| Task.succeed msg)
        |> Task.perform identity


{-| Construct a message that is executed on the next frame. Use this to give
the display time to update.
-}
nextFrame : msg -> Cmd msg
nextFrame =
    sendDelayed 32 Time.millisecond
