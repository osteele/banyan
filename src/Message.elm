module Message exposing (Msg(..), restoreOrSyncFiles, syncFiles)

import Dropbox
import Dropbox.AccountInfo exposing (AccountInfo)
import FilesComponent
import Http
import Model exposing (SortOrder)


type Msg
    = InitializeAccessToken (Maybe String)
    | AuthResponse Dropbox.AuthorizeResult
    | SignIn
    | SignOut
    | SetAccountInfo (Result Http.Error AccountInfo)
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


syncFiles : Msg
syncFiles =
    FilesMessage FilesComponent.StartSyncFiles
