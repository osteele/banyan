module View exposing (..)

import Color
import Data exposing (..)
import Dict
import FileTree exposing (FileTree)
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (attribute, class, href, id)
import Html.Events exposing (onClick)
import Message exposing (..)
import Model exposing (..)
import Utils exposing (..)


config : { color : Maybe Color.Color, githubUrl : String, title : String, description : String }
config =
    { title = "Banyan"
    , description = "A Dropbox file size browser."
    , color = Just Color.blue
    , githubUrl = "https://github.com/osteele/banyan"
    }


view : Model -> Html Msg
view model =
    div []
        [ header model
        , content model
        ]



-- header


header : Model -> Html Msg
header model =
    div [ class "ui top borderless inverted menu" ]
        [ div [ class "ui container grid" ] <|
            List.singleton <|
                row []
                    [ Html.h1 [ class "header item" ] [ text config.title ]
                    , span [ class "item" ] [ text config.description ]
                    , div [ class "right menu" ] <|
                        List.filterMap identity <|
                            [ ifJust (isSignedIn model && not model.files.syncing) <|
                                button
                                    [ class "item", onClick ListFiles ]
                                    [ text "Sync" ]
                            , Just <| signInOut model
                            ]
                    , Html.a
                        [ class "link item"
                        , href config.githubUrl
                        , attribute "target" "_"
                        ]
                        [ icon [ class "large github" ] [] ]
                    ]
        ]


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



-- content


content : Model -> Html Msg
content model =
    div [ class "ui main container" ] <|
        List.filterMap identity <|
            [ flash model
            , ifJust (isSignedIn model) <|
                breadcrumb model
            , Just <|
                grid [ class "two column" ]
                    [ column [] [ treeList model ]
                    , column [] [ treeMap model ]
                    ]
            , ifJust (model.files.requestCount > 0) <|
                progress model
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
        |> List.intersperse sep
        |> hn [ class "ui breadcrumb header" ]


progress : Model -> Html Msg
progress model =
    div
        [ class <|
            "ui message progress"
                ++ (if model.files.syncing then
                        ""
                    else
                        " success"
                   )

        --    data-value data-total
        ]
        [ div [ class "bar" ] [ div [ class "progress" ] [] ]
        , div [ class "label" ]
            [ text <|
                String.join ""
                    [ toStringWithCommas model.files.syncedEntryCount
                    , " entries totalling "
                    , humanize <| FileTree.nodeSize model.files.fileTree
                    , " loaded in "
                    , toString model.files.requestCount
                    , " requests"
                    , if model.files.syncing then
                        "…"
                      else
                        "."
                    ]
            ]
        ]


treeMap : Model -> Html Msg
treeMap model =
    div [ id "treeMap" ] []



-- tree view


toolbar : Model -> Html Msg
toolbar model =
    div
        [ class "toolbar" ]
        [ div [ class "ui mini buttons" ] <|
            List.map
                (\n ->
                    button
                        [ if n == model.depth then
                            class "active"
                          else
                            class ""
                        , class "compact"
                        , onClick <| TreeDepth n
                        ]
                        [ text <| toString n ]
                )
            <|
                [ 1, 2, 3 ]
        , div [ class "ui buttons float-right" ]
            [ sortOrderButton Alphabetic "alphabet ascending" model
            , sortOrderButton DescendingSize "numeric descending" model
            , sortOrderButton AscendingSize "numeric ascending" model
            ]
        ]


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
                    [ subtree model model.depth (Just <| treeListTitle model) <| Model.subtree model ]
            ]


treeListTitle : Model -> Html Msg
treeListTitle =
    breadcrumb_ Html.h3 (div [ class "divider" ] [ text "/" ])


subtree : Model -> Int -> Maybe (Html Msg) -> FileTree -> Html Msg
subtree model depth title tree =
    let
        children =
            if depth > 0 then
                FileTree.nodeChildren tree |> Dict.values
            else
                []

        sort =
            case model.order of
                Alphabetic ->
                    List.sortBy <| FileTree.itemEntry >> .path >> String.toUpper

                AscendingSize ->
                    List.sortBy FileTree.nodeSize

                DescendingSize ->
                    List.sortBy (negate << FileTree.nodeSize)

        childViews =
            if List.isEmpty children then
                []
            else
                [ Html.ul []
                    (children
                        |> sort
                        |> List.map
                            (\st -> Html.li [] [ subtree model (depth - 1) Nothing st ])
                    )
                ]
    in
    case tree of
        FileTree.File entry ->
            div
                [ class "clearfix" ]
                [ text <| takeFileName <| entry.path
                , span [ class "float-right" ] [ text <| humanize <| Maybe.withDefault 0 entry.size ]
                ]

        FileTree.Dir entry size children ->
            div
                []
                (div
                    [ class "clearfix text-primary" ]
                    [ flip Maybe.withDefault title <|
                        Html.a
                            [ onClick <| Focus entry.key ]
                            [ text <| takeFileName entry.path ]
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
