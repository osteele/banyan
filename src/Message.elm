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
    | ListFiles
    | FileList (List FileEntry) Bool
    | FileListError
    | Focus String
    | TreeDepth Int
    | RenderFileTreeMap
