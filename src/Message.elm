module Message exposing (..)

import Data exposing (..)
import Dropbox
import FilesComponent


type Msg
    = AccessToken (Maybe String)
    | AuthResponse Dropbox.AuthorizeResult
    | SignIn
    | SignOut
    | SetAccountInfo AccountInfo
    | FilesMessage FilesMsg
      -- view controls
    | Focus String
    | RenderFileTreeMap
    | SortOrder Data.SortOrder
    | TreeDepth Int


type alias FilesMsg =
    FilesComponent.Msg


restoreOrSyncFiles : Msg
restoreOrSyncFiles =
    FilesMessage FilesComponent.RestoreFromCacheOrListFolder


syncFilesMsg : Msg
syncFilesMsg =
    FilesMessage FilesComponent.ListFolder
