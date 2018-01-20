module View exposing (..)

import BeautifulExample
import Color
import Dict
import FileEntry exposing (..)
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Message exposing (..)
import Model exposing (..)
import Utils exposing (..)


view : Model -> Html Msg
view model =
    div []
        [ header config model
        , BeautifulExample.view config <|
            div [ class "ui main container" ] <|
                List.filterMap identity <|
                    [ Just <|
                        div [ class "ui message" ]
                            [ Html.p [ class "lead" ] [ text <| Maybe.withDefault "" config.details ]
                            ]
                    , ifJust (isSignedIn model) <|
                        Html.h1 [] [ breadcrumb model ]
                    , Just <|
                        div [ class "ui two column grid" ]
                            [ div [ class "column" ]
                                [ listView model ]
                            , div [ class "column" ]
                                [ treeMap model ]
                            ]
                    , ifJust (model.requestCount > 0) <|
                        progress model
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


header : BeautifulExample.Config -> Model -> Html Msg
header config model =
    div [ class "ui top fixed huge borderless inverted menu" ]
        [ div [ class "ui container grid" ] <|
            List.singleton <|
                div [ class "row" ]
                    [ div [ class "header item" ] [ text config.title ]
                    , div [ class "right menu" ] <|
                        List.filterMap identity <|
                            [ ifJust (isSignedIn model) <|
                                toolbar model
                            , ifJust (isSignedIn model && not model.loadingTree) <|
                                button
                                    [ class "item", onClick ListFiles ]
                                    [ text "Sync" ]
                            , Just <| signInOut model
                            ]
                    ]
        ]


signInOut : Model -> Html Msg
signInOut model =
    if isSignedIn model then
        Html.button
            [ class "link item", onClick SignOut ]
            [ Html.i [ class "link sign out icon " ] []
            , text "Sign out"
            ]
    else
        Html.button
            [ class "link item", onClick SignIn ]
            [ Html.i [ class "link sign in icon " ] []
            , text "Sign into Dropbox"
            ]


progress : Model -> Html Msg
progress model =
    div [ class "ui message" ] <|
        List.singleton <|
            text <|
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


treeMap : Model -> Html Msg
treeMap model =
    div [ id "treeMap" ] []


toolbar : Model -> Html Msg
toolbar model =
    div []
        [ span [] [ text "Levels:" ]
        , div [ class "ui buttons" ] <|
            List.map
                (\n ->
                    Html.button
                        [ if n == model.depth then
                            class "ui active button"
                          else
                            class "ui button"
                        , onClick <| TreeDepth n
                        ]
                        [ text <| toString n ]
                )
            <|
                [ 1, 2, 3 ]
        ]


listView : Model -> Html Msg
listView model =
    div [] <|
        List.filterMap identity <|
            [ Just <| div [] [ model.debug |> Maybe.withDefault "" |> text ]
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
                        [ onClick <| Focus prefix ]
                        [ text <|
                            if dir == "" then
                                teamName model
                            else
                                dir
                        ]
                    ]
            )
        |> List.intersperse (Html.i [ class "right angle icon divider" ] [])
        |> Html.h1 [ class "ui breadcrumb header" ]


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
            div
                [ class "clearfix" ]
                [ text <| takeFileName <| entry.path
                , span [ class "float-right" ] [ text <| humanize <| Maybe.withDefault 0 entry.size ]
                ]

        Dir entry size children ->
            div
                []
                (div
                    [ class "clearfix text-primary" ]
                    [ Html.a [ onClick <| Focus entry.key ] [ text <| takeFileName <| entry.path ]
                    , span [ class "float-right" ] [ text <| humanize size ]
                    ]
                    :: childViews
                )


button : List (Html.Attribute msg) -> List (Html msg) -> Html msg
button attrs =
    Html.button (class "ui button" :: attrs)
