module View exposing (..)

import Data exposing (..)
import Dict
import FileEntry
import FileTree exposing (FileTree)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (attribute, class, href, id, style)
import Html.Events exposing (onClick)
import Message exposing (..)
import Model exposing (..)
import Utils exposing (..)


githubURL : String
githubURL =
    "https://github.com/osteele/banyan"


view : Model -> Html Msg
view model =
    div []
        [ header model
        , githubLink
        , if isSignedIn model then
            content model
          else
            startView
        ]



-- header


header : Model -> Html Msg
header model =
    div [ class "ui container" ]
        [ div [ class "ui borderless inverted menu" ]
            [ Html.h1 [ class "header item" ] [ text "Banyan" ]
            , div [ class "right menu" ] <|
                List.filterMap identity <|
                    [ ifJust (isSignedIn model && not model.files.syncing) <|
                        button
                            [ class "item", onClick ListFolder ]
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
    if isSignedIn model then
        div
            [ class "link item", onClick SignOut ]
            [ icon [ class "large link sign out" ] []
            , text "Sign out"
            ]
    else
        div
            [ class "link item", onClick SignIn ]
            [ icon [ class "large link sign in" ] []
            , text "Sign into Dropbox"
            ]


startView : Html Msg
startView =
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



-- content


content : Model -> Html Msg
content model =
    div [ class "ui main container" ] <|
        List.filterMap identity <|
            [ ifJust (model.files.requestCount > 0) <|
                progress model
            , flash model
            , ifJust (isSignedIn model) <|
                breadcrumb model
            , Just <|
                grid [ class "two column" ]
                    [ column [] [ treeList model ]
                    , column [] [ treeMap model ]
                    ]
            ]


flash : Model -> Maybe (Html msg)
flash model =
    model.files.errorMessage
        |> Maybe.map
            (\msg ->
                div [ class "ui warning message" ]
                    [ Html.p [ class "lead" ] [ text msg ]
                    ]
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
            |> FileTree.itemEntry
            |> FileEntry.path
            |> String.split "/"
            |> withPrefixes
            |> List.map
                (\( dir, prefix ) ->
                    div
                        [ class "section" ]
                        [ Html.a
                            [ onClick <| Focus prefix ]
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

        loading =
            files.syncing

        requestsF =
            toFloat files.requestCount

        frac =
            if loading then
                requestsF / (requestsF + 1.0)
            else
                1.0

        width =
            (frac * 100 |> toString |> flip (++) "%")
    in
        div
            [ class <|
                "ui progress"
                    ++ (if loading then
                            ""
                        else
                            " success"
                       )
            ]
            [ div
                [ class "bar"
                , style [ ( "width", width ) ]
                ]
                [ div [ class "progress" ] [] ]
            , div [ class "label" ]
                [ text <|
                    String.join "" <|
                        [ "Loaded "
                        , toStringWithCommas files.syncedEntryCount
                        , " entries totalling "
                        , humanize <| FileTree.nodeSize files.fileTree
                        ]
                            ++ (if loading then
                                    [ " in "
                                    , quantify " request" files.requestCount
                                    , "…"
                                    ]
                                else
                                    [ "."
                                    ]
                               )
                ]
            ]



-- tree map view


treeMap : Model -> Html Msg
treeMap _ =
    div [ id "treeMap" ] []



-- tree list view


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
        , onClick <| TreeDepth depth
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
        , onClick <| SortOrder order
        ]
        [ icon [ class <| "sort " ++ classes ] []
        ]


treeList : Model -> Html Msg
treeList model =
    div [] <|
        List.filterMap identity <|
            [ ifJust (isSignedIn model) <|
                toolbar model
            , ifJust (not <| FileTree.isEmpty model.files.fileTree) <|
                div [ class "tree" ]
                    [ Model.subtree model
                        |> FileTree.trimDepth model.depth
                        |> FileTree.combineSmallerEntries 20 2
                        |> subtree model (Just <| treeListTitle model)
                    ]
            ]


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
                    List.sortBy <| FileTree.itemEntry >> FileEntry.path >> String.toUpper

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
            FileTree.File entry ->
                div
                    [ class "clearfix" ]
                    [ text <| takeFileName <| FileEntry.path entry
                    , span [ class "float-right" ]
                        [ text <| humanize <| Maybe.withDefault 0 <| FileEntry.size entry ]
                    ]

            FileTree.Dir entry size _ ->
                div
                    []
                    (div
                        [ class "clearfix text-primary" ]
                        [ flip Maybe.withDefault title <|
                            Html.a
                                [ onClick <| Focus <| FileEntry.key entry ]
                                [ text <| takeFileName <| FileEntry.path entry ]
                        , span [ class "float-right" ] [ text <| humanize size ]
                        ]
                        :: childViews
                    )



--- helpers


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
