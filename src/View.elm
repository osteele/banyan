module View exposing (..)

import Dict
import FileEntry exposing (..)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Message exposing (..)
import Model exposing (..)
import Utils exposing (..)


view : Model -> Html Msg
view model =
    div [] <|
        List.filterMap identity <|
            [ ifJust (isSignedIn model) <|
                div [] [ text (model.accountInfo |> Maybe.map .name |> Maybe.map .display_name |> Maybe.withDefault "") ]
            , ifJust (isSignedIn model) <|
                Html.button
                    [ Html.Events.onClick <| SignOut ]
                    [ Html.text <| "Sign out" ]
            , ifJust (not <| isSignedIn model) <|
                Html.button
                    [ Html.Events.onClick <| SignIn ]
                    [ Html.text <| "Sign into Dropbox" ]
            , ifJust (isSignedIn model && not model.loadingTree) <|
                Html.button
                    [ Html.Events.onClick ListFiles ]
                    [ Html.text "Sync" ]
            , Just <| div [ id "treeMap" ] []
            , ifJust (isSignedIn model) <|
                div []
                    [ Html.span [] [ text "Levels:" ]
                    , Html.button [ Html.Events.onClick <| TreeDepth 1 ] [ text "1" ]
                    , Html.button [ Html.Events.onClick <| TreeDepth 2 ] [ text "2" ]
                    , Html.button [ Html.Events.onClick <| TreeDepth 3 ] [ text "3" ]
                    ]
            , ifJust (model.requestCount > 0) <|
                div []
                    [ text <|
                        String.join ""
                            [ toStringWithCommas model.loadedEntryCount
                            , " entries totalling "
                            , humanize <| nodeSize model.fileTree
                            , " loaded in "
                            , toString model.requestCount
                            , " requests"
                            , if model.loadingTree then
                                "…"
                              else
                                "."
                            ]
                    ]
            , Just <| div [] [ model.debug |> Maybe.withDefault "" |> text ]
            , ifJust (FileEntry.isEmpty model.fileTree |> not) <|
                treeView
                    (model.accountInfo |> Maybe.map .teamName |> Maybe.withDefault "Personal")
                    model.depth
                    (model.fileTree |> getSubtree model.path |> Maybe.withDefault model.fileTree)
            ]


treeView : String -> Int -> FileTree -> Html Msg
treeView teamName depth tree =
    let
        breadcrumbs path =
            Html.ul
                [ class "breadcrumb" ]
                (path
                    |> String.split "/"
                    |> (\dirs ->
                            prefixes dirs
                                |> List.map (String.join "/")
                                |> zip dirs
                       )
                    |> List.map
                        (\( dir, prefix ) ->
                            Html.li
                                [ Html.Events.onClick <| Focus prefix
                                , class "folder"
                                ]
                                [ text <|
                                    if dir == "" then
                                        teamName
                                    else
                                        dir
                                ]
                        )
                )
    in
    treeView_ (Just <| Html.h2 [] <| List.singleton <| breadcrumbs <| .path <| itemEntry tree) depth tree


treeView_ : Maybe (Html Msg) -> Int -> FileTree -> Html Msg
treeView_ title depth tree =
    let
        childViews children =
            Html.ul []
                (Dict.values children
                    |> List.sortBy (itemEntry >> .path >> String.toUpper)
                    |> List.map
                        (\t -> Html.li [] [ treeView_ Nothing (depth - 1) t ])
                )
    in
    case tree of
        File entry ->
            Html.div
                [ class "file-item" ]
                [ text <| takeFileName <| entry.path
                , Html.span [ class "size" ] [ text <| humanize <| Maybe.withDefault 0 entry.size ]
                ]

        Dir entry size children ->
            Html.div
                []
                [ Html.a
                    [ class "folder file-item" ]
                    [ Maybe.withDefault (Html.a [ Html.Events.onClick <| Focus entry.key ] [ text <| takeFileName <| entry.path ]) title
                    , Html.span [ class "size" ] [ text <| humanize size ]
                    ]
                , if depth > 0 then
                    childViews children
                  else
                    div [] []
                ]
