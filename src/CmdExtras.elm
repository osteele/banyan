module CmdExtras exposing (..)

import Process
import Task
import Time exposing (Time)


sendDelayed : Time -> Float -> msg -> Cmd msg
sendDelayed time unit msg =
    Process.sleep (time * unit)
        |> Task.andThen (always <| Task.succeed msg)
        |> Task.perform identity


message : msg -> Cmd msg
message =
    Task.perform identity << Task.succeed


nextFrame : msg -> Cmd msg
nextFrame =
    sendDelayed 32 Time.millisecond
