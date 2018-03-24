module Message exposing (..)

import Dropbox
import Dropbox.AccountInfo exposing (AccountInfo)
import FilesComponent
import Model exposing (SortOrder)


type Msg
    = InitializeAccessToken (Maybe String)
    | AuthResponse Dropbox.AuthorizeResult
    | SignIn
    | SignOut
    | SetAccountInfo AccountInfo
    | FilesMessage FilesMsg
      -- view controls
    | RenderFileTreeMap
    | DismissMessageView Int
    | SetFocus String
    | SetSortOrder SortOrder
    | SetTreeDepth Int


type alias FilesMsg =
    FilesComponent.Msg


restoreOrSyncFiles : AccountInfo -> Msg
restoreOrSyncFiles =
    FilesMessage << FilesComponent.RestoreFromCacheOrListFolder


syncFilesMsg : Msg
syncFilesMsg =
    FilesMessage FilesComponent.ListFolder
