module View exposing (..)

import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
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
    Grid.container []
        [ Grid.row []
            [ Grid.col []
                [ view_ model ]
            ]
        ]


button a =
    Button.button [ Button.primary, Button.attrs a ]


button2 a =
    Button.button [ Button.secondary, Button.attrs a ]


view_ : Model -> Html Msg
view_ model =
    div [] <|
        List.filterMap identity <|
            [ ifJust (isSignedIn model) <|
                div [] [ text (model.accountInfo |> Maybe.map .name |> Maybe.map .display_name |> Maybe.withDefault "") ]
            , ifJust (isSignedIn model) <|
                button
                    [ Html.Events.onClick <| SignOut ]
                    [ Html.text <| "Sign out" ]
            , ifJust (not <| isSignedIn model) <|
                button
                    [ Html.Events.onClick <| SignIn ]
                    [ Html.text <| "Sign into Dropbox" ]
            , ifJust (isSignedIn model && not model.loadingTree) <|
                button
                    [ Html.Events.onClick ListFiles ]
                    [ Html.text "Sync" ]
            , Just <| div [ id "treeMap" ] []
            , ifJust (isSignedIn model) <|
                div []
                    [ Html.span [] [ text "Levels:" ]
                    , div [ class "btn-group" ] <|
                        List.map
                            (\n ->
                                Button.button
                                    [ if n == model.depth then
                                        Button.primary
                                      else
                                        Button.secondary
                                    , Button.attrs [ Html.Events.onClick <| TreeDepth n ]
                                    ]
                                    [ text <| toString n ]
                            )
                        <|
                            [ 1, 2, 3 ]
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
            , ifJust (not <| FileEntry.isEmpty model.fileTree) <|
                treeView
                    (model.accountInfo |> Maybe.map .teamName |> Maybe.withDefault "Personal")
                    model.depth
                    (model.fileTree |> getSubtree model.path |> Maybe.withDefault model.fileTree)
            ]


breadcrumb : String -> FileTree -> Html Msg
breadcrumb teamName tree =
    let
        withPrefixes dirs =
            prefixes dirs
                |> List.map (String.join "/")
                |> zip dirs
    in
    tree
        |> itemEntry
        |> .path
        |> String.split "/"
        |> withPrefixes
        |> List.map
            (\( dir, prefix ) ->
                Html.li
                    [ class "breadcrumb-item" ]
                    [ Html.a
                        [ Html.Events.onClick <| Focus prefix ]
                        [ text <|
                            if dir == "" then
                                teamName
                            else
                                dir
                        ]
                    ]
            )
        |> Html.ul [ class "breadcrumb" ]


treeView : String -> Int -> FileTree -> Html Msg
treeView teamName depth tree =
    let
        header =
            Html.h2 [] [ breadcrumb teamName tree ]
    in
    treeView_ Nothing depth tree
        |> List.singleton
        |> div [ class "tree" ]
        |> (\v -> div [] [ header, v ])


treeView_ : Maybe (Html Msg) -> Int -> FileTree -> Html Msg
treeView_ title depth tree =
    let
        children =
            if depth > 0 then
                nodeChildren tree |> Dict.values
            else
                []

        childViews =
            if List.isEmpty children then
                []
            else
                [ Html.ul []
                    (children
                        |> List.sortBy (itemEntry >> .path >> String.toUpper)
                        |> List.map
                            (\t -> Html.li [] [ treeView_ Nothing (depth - 1) t ])
                    )
                ]
    in
    case tree of
        File entry ->
            Html.div
                [ class "clearfix" ]
                [ text <| takeFileName <| entry.path
                , Html.span [ class "float-right" ] [ text <| humanize <| Maybe.withDefault 0 entry.size ]
                ]

        Dir entry size children ->
            Html.div
                []
                (Html.div
                    [ class "clearfix text-primary" ]
                    [ Maybe.withDefault (Html.a [ Html.Events.onClick <| Focus entry.key ] [ text <| takeFileName <| entry.path ]) title
                    , Html.span [ class "float-right" ] [ text <| humanize size ]
                    ]
                    :: childViews
                )
