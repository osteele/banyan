module View exposing (..)

import Date
import Date.Extra as Date
import Dropbox.FileTree as FileTree exposing (FileTree(..))
import Dict
import Extras exposing (..)
import FilesComponent exposing (State(..), isSyncing)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (attribute, class, href, id, style)
import Html.Events exposing (onClick)
import Message exposing (..)
import Model exposing (..)


view : Model -> Html Msg
view model =
    div []
        [ header model
        , githubLink
        , if isSignedOut model then
            signedOut
          else
            content model
        ]


githubURL : String
githubURL =
    "https://github.com/osteele/banyan"



-- HEADER


header : Model -> Html Msg
header model =
    div [ class "ui container" ]
        [ div [ class "ui borderless inverted menu" ]
            [ Html.h1 [ class "header item" ] [ text "Banyan" ]
            , div [ class "right menu" ] <|
                List.filterMap identity <|
                    [ ifJust (isSignedIn model && not (isSyncing model.files)) <|
                        button
                            [ class "item", onClick syncFiles ]
                            [ text "Sync" ]
                    , Just <| signInOut model
                    ]
            ]
        ]


githubLink : Html msg
githubLink =
    Html.a
        [ class "github link item"
        , href githubURL
        , attribute "target" "_"
        ]
        [ icon [ class "huge github" ] [] ]


signInOut : Model -> Html Msg
signInOut model =
    case model.status of
        SignedIn ->
            div
                [ class "link item", onClick SignOut ]
                [ icon [ class "large link sign out" ] []
                , text "Sign out"
                ]

        SignedOut ->
            div
                [ class "link item", onClick SignIn ]
                [ icon [ class "large link sign in" ] []
                , text "Sign into Dropbox"
                ]

        SigningIn ->
            div [ class "item" ] [ text "Signing into Dropbox…" ]


signedOut : Html Msg
signedOut =
    div [ class "ui text container" ]
        [ Html.p []
            [ Html.a [ class "link item", onClick SignIn ] [ text "Sign in" ]
            , text " to Dropbox to browse files and folders by size."
            ]
        , Html.img
            [ class "ui big rounded bordered fluid image"
            , Html.Attributes.src "/screenshot-view.png"
            ]
            []
        ]



-- CONTENT


content : Model -> Html Msg
content model =
    div [ class "ui main container" ]
        [ errors model
        , if FilesComponent.isEmpty model.files then
            div
                [ class "ui segment", style [ ( "min-height", "500px" ) ] ]
                [ div [ class "ui active inverted dimmer" ]
                    [ div [ class "ui text loader" ]
                        [ text "Loading file entries…" ]
                    ]
                , Html.p [] []
                ]
          else
            div []
                [ progress model
                , if FilesComponent.isEmpty model.files then
                    div [] []
                  else
                    breadcrumb model
                , grid [ class "two column" ]
                    [ column [] [ treeList model ]
                    , column [] [ treeMap model ]
                    ]
                ]
        ]


errors : Model -> Html Msg
errors model =
    div []
        (model.errors
            |> List.indexedMap
                (\i msg ->
                    div [ class "ui warning message" ]
                        [ icon [ class "close", onClick <| DismissMessageView i ] []
                        , Html.p [ class "lead" ] [ text msg ]
                        ]
                )
        )


breadcrumb : Model -> Html Msg
breadcrumb =
    breadcrumb_ Html.h1 <| icon [ class "right angle icon divider" ] []


breadcrumb_ : Node Msg -> Html Msg -> Model -> Html Msg
breadcrumb_ hn sep model =
    let
        withPrefixes dirs =
            prefixes dirs
                |> List.map (String.join "/")
                |> zip dirs
    in
        model
            |> Model.subtree
            |> FileTree.nodePath
            |> String.split "/"
            |> withPrefixes
            |> List.map
                (\( dir, prefix ) ->
                    div
                        [ class "section" ]
                        [ Html.a
                            [ onClick <| SetFocus prefix ]
                            [ text <|
                                if dir == "" then
                                    teamName model
                                else
                                    dir
                            ]
                        ]
                )
            |> List.intersperse sep
            |> hn [ class "ui breadcrumb header" ]


progress : Model -> Html Msg
progress model =
    let
        files =
            model.files

        width =
            (FilesComponent.completion files |> (*) 100 |> toString) ++ "%"

        msg =
            case files.state of
                Syncing { entries, requests } ->
                    text <|
                        String.join "" <|
                            [ "Loaded "
                            , toStringWithCommas entries
                            , " in "
                            , quantify "request" requests
                            , "…"
                            ]

                Synced { entries, requests } ->
                    text <|
                        String.join "" <|
                            [ "Loaded "
                            , toStringWithCommas entries
                            , " in "
                            , quantify "request" requests
                            , "."
                            ]

                Unsynced ->
                    text "Unsyced"

                StartedSync ->
                    text "Starting sync…"

                Decoding _ ->
                    text "Loading file entries from cache…"

                FromCache timestamp ->
                    span []
                        [ text "Cached at "
                        , text <| Date.toFormattedString "h:mm a on EEEE, MMMM d, y" <| Date.fromTime timestamp
                        , text ". "
                        , Html.a
                            [ class "item", onClick syncFiles ]
                            [ text "Re-sync now" ]
                        , text "."
                        ]

        progressView classes width msg =
            div
                [ class <| String.join " " <| "ui progress" :: classes ]
                [ div
                    [ class "bar"
                    , style [ ( "width", width ) ]
                    ]
                    [ div [ class "progress" ] [] ]
                , div [ class "label" ] [ msg ]
                ]
    in
        progressView
            (if isSyncing files then
                []
             else
                [ "success" ]
            )
            width
            msg



-- TREE MAP


treeMap : Model -> Html Msg
treeMap _ =
    div [ id "treeMap" ] []



-- TREE LIST


toolbar : Model -> Html Msg
toolbar model =
    div
        [ class "toolbar" ]
        [ div [ class "ui buttons" ] <|
            [ button [ class "disabled" ] [ text "Levels" ] ]
                ++ List.map (foldDepthButton model)
                    [ 1, 2, 3 ]
        , div [ class "right floated ui buttons" ]
            [ button [ class "disabled" ] [ text "Sort" ]
            , sortOrderButton Alphabetic "alphabet ascending" model
            , sortOrderButton DescendingSize "numeric descending" model
            , sortOrderButton AscendingSize "numeric ascending" model
            ]
        ]


foldDepthButton : Model -> Int -> Html Msg
foldDepthButton model depth =
    button
        [ if depth == model.depth then
            class "active"
          else
            class ""
        , class "compact"
        , onClick <| SetTreeDepth depth
        ]
        [ text <| toString depth ]


sortOrderButton : SortOrder -> String -> Model -> Html Msg
sortOrderButton order classes model =
    button
        [ class <|
            "icon"
                ++ (if order == model.order then
                        " active"
                    else
                        ""
                   )
        , onClick <| SetSortOrder order
        ]
        [ icon [ class <| "sort " ++ classes ] []
        ]


treeList : Model -> Html Msg
treeList model =
    ifDiv (not <| FileTree.isEmpty model.files.files)
        []
        [ toolbar model
        , div [ class "tree" ]
            [ Model.subtree model
                |> FileTree.trimDepth model.depth
                |> FileTree.combineSmallerEntries 20 2
                |> subtree model (Just <| treeListTitle model)
            ]
        ]


ifDiv : Bool -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
ifDiv condition =
    if condition then
        div
    else
        \_ _ -> div [] []


treeListTitle : Model -> Html Msg
treeListTitle =
    breadcrumb_ Html.h3 (div [ class "divider" ] [ text "/" ])


subtree : Model -> Maybe (Html Msg) -> FileTree -> Html Msg
subtree model title tree =
    let
        children =
            FileTree.nodeChildren tree |> Dict.values

        sort =
            case model.order of
                Alphabetic ->
                    List.sortBy <| FileTree.nodePath >> String.toLower

                AscendingSize ->
                    List.sortBy FileTree.nodeSize

                DescendingSize ->
                    List.sortBy <| negate << FileTree.nodeSize

        childViews =
            if List.isEmpty children then
                []
            else
                [ Html.ul [] <|
                    (children
                        |> sort
                        |> List.map
                            (\st -> Html.li [] [ subtree model Nothing st ])
                    )
                ]
    in
        case tree of
            File { path, size } ->
                div
                    [ class "clearfix" ]
                    [ text <| takeFileName path
                    , span [ class "float-right" ]
                        [ text <| humanize size ]
                    ]

            Folder { path } size _ ->
                div
                    []
                    (div
                        [ class "clearfix text-primary" ]
                        [ flip Maybe.withDefault title <|
                            Html.a
                                [ onClick <| SetFocus path ]
                                [ text <| takeFileName path ]
                        , span [ class "float-right" ] [ text <| humanize size ]
                        ]
                        :: childViews
                    )



--- HELPERS


type alias Node msg =
    List (Html.Attribute msg) -> List (Html msg) -> Html msg


button : Node msg
button attrs =
    Html.button <| class "ui button" :: attrs


column : Node msg
column attrs =
    div <| class "column" :: attrs


grid : Node msg
grid attrs =
    div <| class "ui grid" :: attrs


icon : Node msg
icon attrs =
    Html.i <| class "icon" :: attrs


row : Node msg
row attrs =
    div <| class "row" :: attrs
