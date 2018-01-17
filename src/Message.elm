module Message exposing (..)

import Data exposing (..)
import Dropbox
import FileEntry exposing (..)


type Msg
    = SignIn
    | SignOut
    | ClientID String
    | AuthResponse Dropbox.AuthorizeResult
    | SetAccountInfo AccountInfo
    | AccessToken (Maybe String)
    | ListFiles
    | FileList (List FileEntry) Bool
    | FileListError
    | Focus String
    | TreeDepth Int
    | RenderFileTreeMap
