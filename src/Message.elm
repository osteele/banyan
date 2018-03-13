module Message exposing (..)

import Data exposing (..)
import Dropbox
import FileEntry exposing (..)


type Msg
    = AccessToken (Maybe String)
    | AuthResponse Dropbox.AuthorizeResult
    | SignIn
    | SignOut
    | SetAccountInfo AccountInfo
      -- file retrieval
    | ListFolder
    | ReceiveListFolderResponse (Result String ( List FileEntry, Bool ))
    | RestoreFromCacheOrListFolder
    | RestoreFromCache
      -- view controls
    | Focus String
    | RenderFileTreeMap
    | SortOrder Data.SortOrder
    | TreeDepth Int
