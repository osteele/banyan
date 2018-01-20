module View exposing (..)

import BeautifulExample
import Color
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
    Html.div []
        [ banner model
        , BeautifulExample.view config <|
            div []
                [ headerStuff model
                , div [ class "ui two column grid" ]
                    [ div [ class "column" ]
                        [ listView model ]
                    , div [ class "column" ]
                        [ treeMap model ]
                    ]
                ]
        ]


config : BeautifulExample.Config
config =
    { title = "Banyan"
    , details = Just "Dropbox file size browser."
    , color = Just Color.blue
    , maxWidth = 1200
    , githubUrl = Just "https://github.com/osteele/banyan"
    , documentationUrl = Nothing
    }


banner : Model -> Html Msg
banner model =
    div [] <|
        List.filterMap identity <|
            [ ifJust (isSignedIn model) <|
                button
                    [ Html.Events.onClick <| SignOut ]
                    [ Html.text <| "Sign out" ]
            , ifJust (not <| isSignedIn model) <|
                button
                    [ Html.Events.onClick <| SignIn ]
                    [ Html.text <| "Sign into Dropbox" ]
            ]


headerStuff : Model -> Html Msg
headerStuff model =
    div [] <|
        List.filterMap identity <|
            [ ifJust (isSignedIn model && not model.loadingTree) <|
                button
                    [ Html.Events.onClick ListFiles ]
                    [ Html.text "Sync" ]
            , ifJust (model.requestCount > 0) <|
                statsView model
            , ifJust (isSignedIn model) <|
                Html.h1 [] [ breadcrumb model ]
            ]


statsView : Model -> Html msg
statsView model =
    div [ class "ui segment" ]
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


treeMap : Model -> Html Msg
treeMap model =
    div [ id "treeMap" ] []


listView : Model -> Html Msg
listView model =
    div [] <|
        List.filterMap identity <|
            [ ifJust (isSignedIn model) <|
                div []
                    [ Html.span [] [ text "Levels:" ]
                    , div [ class "ui buttons" ] <|
                        List.map
                            (\n ->
                                Html.button
                                    [ if n == model.depth then
                                        class "ui active button"
                                      else
                                        class "ui button"
                                    , Html.Events.onClick <| TreeDepth n
                                    ]
                                    [ text <| toString n ]
                            )
                        <|
                            [ 1, 2, 3 ]
                    ]
            , Just <| div [] [ model.debug |> Maybe.withDefault "" |> text ]
            , ifJust (not <| FileEntry.isEmpty model.fileTree) <|
                treeView model
            ]


breadcrumb : Model -> Html Msg
breadcrumb model =
    let
        withPrefixes dirs =
            prefixes dirs
                |> List.map (String.join "/")
                |> zip dirs
    in
    model
        |> subtree
        |> itemEntry
        |> .path
        |> String.split "/"
        |> withPrefixes
        |> List.map
            (\( dir, prefix ) ->
                div
                    [ class "section" ]
                    [ Html.a
                        [ Html.Events.onClick <| Focus prefix ]
                        [ text <|
                            if dir == "" then
                                teamName model
                            else
                                dir
                        ]
                    ]
            )
        |> List.intersperse (Html.i [ class "right angle icon divider" ] [])
        |> div [ class "ui breadcrumb" ]


treeView : Model -> Html Msg
treeView model =
    treeView_ model.depth (subtree model)
        |> List.singleton
        |> div [ class "tree" ]


treeView_ : Int -> FileTree -> Html Msg
treeView_ depth tree =
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
                            (\t -> Html.li [] [ treeView_ (depth - 1) t ])
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
                    [ Html.a [ Html.Events.onClick <| Focus entry.key ] [ text <| takeFileName <| entry.path ]
                    , Html.span [ class "float-right" ] [ text <| humanize size ]
                    ]
                    :: childViews
                )


button : List (Html.Attribute msg) -> List (Html msg) -> Html msg
button attrs =
    Html.button (class "ui button" :: attrs)
