port module Dropbox.AccountInfo exposing (..)

import Dropbox exposing (..)
import Extras exposing (..)
import Regex


-- RECORDS
-- TODO make accountType an enum


type alias AccountInfo =
    { accountId : String
    , accountType : String
    , country : String
    , disabled : Bool
    , email : String
    , emailVerified : Bool
    , isPaired : Bool
    , locale : String
    , referralLink : String
    , name : Name
    , team : Maybe Team
    , rootInfo : RootInfo
    }


type alias Name =
    { abbreviatedName : String
    , displayName : String
    , familiarName : String
    , givenName : String
    , surname : String
    }


type alias RootInfo =
    { --- TODO tag
      rootNamespaceId : String
    , homeNamespaceId : String
    }


type alias Team =
    { id : String
    , name : String

    -- TODO officeAddinPolicy
    -- TODO SharingPolicies
    }


type alias SharingPolicies =
    { sharedFolderMemberPolicy : String
    , sharedFolderJoinPolicy : String
    , sharedLinkCreatePolicy : String
    }



-- ACCESS TOKEN


bearerRegex : Regex.Regex
bearerRegex =
    Regex.regex "Bearer \"(.+)\""


extractAccessToken : UserAuth -> Maybe String
extractAccessToken auth =
    -- TODO extract from JSON instead?
    auth |> Basics.toString |> firstMatch bearerRegex



-- PORTS


port getAccountInfo : String -> Cmd msg


port receiveAccountInfo : (AccountInfo -> msg) -> Sub msg
