module DropboxUtils exposing (..)

import Dropbox
import Regex
import Utils exposing (firstMatch)


extractAccessToken : Dropbox.UserAuth -> Maybe String
extractAccessToken auth =
    -- TODO extract from JSON instead?
    auth |> toString |> firstMatch bearerRegex


bearerRegex : Regex.Regex
bearerRegex =
    Regex.regex "Bearer \"(.+)\""
