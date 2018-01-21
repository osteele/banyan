module View exposing (..)

import Color
import Dict
import FileEntry exposing (..)
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Message exposing (..)
import Model exposing (..)
import Utils exposing (..)


config : { color : Maybe Color.Color, githubUrl : String, title : String, description : String }
config =
    { title = "Banyan"
    , description = "A dropbox file size browser."
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
    div [ class "ui top  huge borderless inverted menu" ]
        [ div [ class "ui container grid" ] <|
            List.singleton <|
                row []
                    [ Html.h1 [ class "header item" ] [ text config.title ]
                    , span [ class " item" ] [ text config.description ]
                    , div [ class "right menu" ] <|
                        List.filterMap identity <|
                            [ ifJust (isSignedIn model && not model.loadingTree) <|
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
            [ icon [ class "link sign out icon" ] []
            , text "Sign out"
            ]
    else
        Html.button
            [ class "link item", onClick SignIn ]
            [ icon [ class "link sign in icon " ] []
            , text "Sign into Dropbox"
            ]



-- content


content : Model -> Html Msg
content model =
    div [ class "ui main container" ] <|
        List.filterMap identity <|
            [ flash model
            , ifJust (isSignedIn model) <|
                Html.h1 [] [ breadcrumb model ]
            , Just <|
                grid [ class "two column" ]
                    [ column [] [ listView model ]
                    , column [] [ treeMap model ]
                    ]
            , ifJust (model.requestCount > 0) <|
                progress model
            ]


flash : { a | debug : Maybe String } -> Maybe (Html msg)
flash model =
    model.debug
        |> Maybe.map
            (\message ->
                div [ class "ui message" ]
                    [ Html.p [ class "lead" ] [ text message ]
                    ]
            )


breadcrumb : Model -> Html Msg
breadcrumb model =
    let
        withPrefixes dirs =
            prefixes dirs
                |> List.map (String.join "/")
                |> zip dirs
    in
    model
        |> Model.subtree
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
        |> List.intersperse (icon [ class "right angle icon divider" ] [])
        |> Html.h1 [ class "ui breadcrumb header" ]


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



-- tree view


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
            [ ifJust (isSignedIn model) <|
                toolbar model
            , ifJust (not <| FileEntry.isEmpty model.fileTree) <|
                treeView model
            ]


treeView : Model -> Html Msg
treeView model =
    subtree model.depth (Model.subtree model)
        |> List.singleton
        |> div [ class "tree" ]


subtree : Int -> FileTree -> Html Msg
subtree depth tree =
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
                            (\t -> Html.li [] [ subtree (depth - 1) t ])
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



--- helpers


button : List (Html.Attribute msg) -> List (Html msg) -> Html msg
button attrs =
    Html.button (class "ui button" :: attrs)


column : List (Html.Attribute msg) -> List (Html msg) -> Html msg
column attrs =
    div (class "column" :: attrs)


grid : List (Html.Attribute msg) -> List (Html msg) -> Html msg
grid attrs =
    div (class "ui grid" :: attrs)


icon : List (Html.Attribute msg) -> List (Html msg) -> Html msg
icon attrs =
    Html.i (class "icon" :: attrs)


row : List (Html.Attribute msg) -> List (Html msg) -> Html msg
row attrs =
    div (class "row" :: attrs)
