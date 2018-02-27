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
    | SyncFiles
    | ReceiveListFolderResponse (List FileEntry) Bool
    | SyncFilesError (Maybe String)
    | RenderFileTreeMap
      -- view controls
    | Focus String
    | SortOrder Data.SortOrder
    | TreeDepth Int
