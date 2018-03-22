module Message exposing (..)

import AccountInfo exposing (..)
import Dropbox
import FilesComponent
import Model exposing (SortOrder)


type Msg
    = AccessToken (Maybe String)
    | AuthResponse Dropbox.AuthorizeResult
    | SignIn
    | SignOut
    | SetAccountInfo AccountInfo
    | FilesMessage FilesMsg
      -- view controls
    | DismissMessageView Int
    | Focus String
    | RenderFileTreeMap
    | SortOrder SortOrder
    | TreeDepth Int


type alias FilesMsg =
    FilesComponent.Msg


restoreOrSyncFiles : Msg
restoreOrSyncFiles =
    FilesMessage FilesComponent.RestoreFromCacheOrListFolder


syncFilesMsg : Msg
syncFilesMsg =
    FilesMessage FilesComponent.ListFolder
