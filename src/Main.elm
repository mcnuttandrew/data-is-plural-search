module Main exposing (..)

import Browser
import Compare exposing (Comparator)
import Csv
import Date exposing (Date, fromIsoString, fromOrdinalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode



-- Main


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- Model


type alias Model =
    { data : Maybe (List DataRow)
    , selectedRows : List DataRow
    , search : String
    , sort : SortType
    , pages : Int
    }


type alias DataRow =
    { edition : String
    , editionDate : Date
    , position : Int
    , headline : String
    , text : String
    , links : List String
    , hattips : List String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { data = Nothing, search = "", selectedRows = [], sort = ByDate, pages = 1 }
    , Http.get
        { url = "./latest.tsv"
        , expect = Http.expectString GotText
        }
    )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateR msg model, Cmd.none )


getPosition : String -> Int
getPosition x =
    case String.toInt x of
        Just val ->
            val

        Nothing ->
            -1


getDate : String -> Date
getDate str =
    case fromIsoString (String.replace "." "-" str) of
        Ok date ->
            date

        Err _ ->
            fromOrdinalDate 1970 1


type SortType
    = ByDate
    | ByName
    | ByPosition


type alias ScrollInfo =
    { scrollHeight : Float
    , scrollTop : Float
    , offsetHeight : Float
    }



-- nb: https://stacktracehq.com/blog/comparing-and-sorting-in-elm/


compareByDate : Comparator DataRow
compareByDate a b =
    Date.compare a.editionDate b.editionDate


negateOrder : Order -> Order
negateOrder x =
    case x of
        GT ->
            LT

        EQ ->
            EQ

        LT ->
            GT


compareByPosition : Comparator DataRow
compareByPosition a b =
    compare a.position b.position


sortList : List DataRow -> SortType -> List DataRow
sortList data sort =
    let
        comparator a b =
            case sort of
                ByDate ->
                    negateOrder (compareByDate a b)

                ByName ->
                    compare a.headline b.headline

                ByPosition ->
                    compareByPosition a b
    in
    List.sortWith comparator data


parseRow : List String -> List DataRow -> List DataRow
parseRow row acc =
    case row of
        [ edition, position, headline, text, links, hattips ] ->
            { edition = edition
            , editionDate = getDate edition
            , position = getPosition position
            , headline = headline
            , text = text
            , links = String.split " " links
            , hattips = String.split " " hattips
            }
                :: acc

        [ edition, position, headline, text, links ] ->
            { edition = edition
            , editionDate = getDate edition
            , position = getPosition position
            , headline = headline
            , text = text
            , links = String.split " " links
            , hattips = []
            }
                :: acc

        _ ->
            acc


produceData : String -> SortType -> Maybe (List DataRow)
produceData inputStr sort =
    Just
        (sortList
            (List.foldl parseRow
                []
                (Csv.parseWith "\t" inputStr).records
            )
            sort
        )


withLog : a -> String -> a
withLog x msg =
    -- let
    --     dummy =
    --         Debug.log "LOG" msg
    -- in
    x


applySearchAndSort : String -> Maybe (List DataRow) -> SortType -> List DataRow
applySearchAndSort search data sort =
    let
        str =
            String.toLower search

        searchPred row =
            String.contains str (String.toLower row.text)
                || String.contains str (String.toLower row.headline)
    in
    case data of
        Just rows ->
            sortList (List.filter searchPred rows) sort

        Nothing ->
            []


receiveText : String -> Model -> Model
receiveText fullText model =
    let
        data =
            produceData fullText model.sort

        output =
            case data of
                Just rows ->
                    rows

                Nothing ->
                    []
    in
    { model | data = data, selectedRows = output }


onScroll msg =
    on "scroll" (Json.Decode.map msg scrollInfoDecoder)


scrollInfoDecoder =
    Json.Decode.map3 ScrollInfo
        (Json.Decode.at [ "target", "scrollHeight" ] Json.Decode.float)
        (Json.Decode.at [ "target", "scrollTop" ] Json.Decode.float)
        (Json.Decode.at [ "target", "offsetHeight" ] Json.Decode.float)


updateR : Msg -> Model -> Model
updateR msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    receiveText fullText model

                Err e ->
                    -- let
                    --     dummy =
                    --         Debug.log "errror" e
                    -- in
                    model

        SpecifySearch search ->
            { model | search = search, selectedRows = applySearchAndSort search model.data model.sort, pages = 1 }

        SetPosition sort ->
            { model | sort = sort, selectedRows = applySearchAndSort model.search model.data sort, pages = 1 }

        -- https://stackoverflow.com/questions/40690998/how-to-implement-infinite-scroll-in-elm
        ScrollEvent { scrollHeight, scrollTop, offsetHeight } ->
            if (scrollHeight - scrollTop) <= offsetHeight then
                { model | pages = model.pages + 1 }

            else
                model


type Msg
    = GotText (Result Http.Error String)
    | SpecifySearch String
    | SetPosition SortType
    | ScrollEvent ScrollInfo



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- View


view : Model -> Html Msg
view { selectedRows, search, sort, pages } =
    div [ id "app", onScroll ScrollEvent ]
        [ div [ class "header" ]
            [ div [ class "header-contents" ]
                [ h1 []
                    [ text "Data Is Plural Search" ]
                , p []
                    [ span []
                        [ text "Data is plural is a " ]
                    , a
                        [ href "https://tinyletter.com/data-is-plural" ]
                        [ text " weekly newsletter" ]
                    , span
                        []
                        [ text " of useful/curious datasets published by " ]
                    , a
                        [ href "https://twitter.com/jsvine" ]
                        [ text "Jeremy Singer-Vine" ]
                    , span []
                        [ text ". You can find out more about his project at " ]
                    , a
                        [ href "https://tinyletter.com/data-is-plural" ]
                        [ text "the project page" ]
                    , span []
                        [ text ". This is an unofficial page that presents the " ]
                    , a
                        [ href "https://docs.google.com/spreadsheets/d/1wZhPLMCHKJvwOkP4juclhjFgqIY8fQFMemwKL2c64vk/edit#gid=0" ]
                        [ text "full archive" ]
                    , span []
                        [ text " of the news letter in an easy to search and browse manner. It is not affiliated with Singer-Vine. Please enjoy! " ]
                    , a
                        [ href "https://github.com/mcnuttandrew/data-is-plural-search" ]
                        [ text "PR/Issues/Comments welcome" ]
                    ]
                ]
            , div [ class "flex" ]
                [ input [ placeholder "Search here", onInput SpecifySearch, value search ] []
                , span [ class "margin-left" ] [ text "Sort:" ]
                , renderSortButton "By Date" ByDate sort
                , renderSortButton "By Name" ByName sort
                , renderSortButton "By Position" ByPosition sort
                ]
            ]
        , div [ class "entries" ] (List.map renderRow (List.take (20 * pages) selectedRows))
        , if List.length selectedRows == 0 then
            div [ class "loader" ] []

          else
            div [] []
        ]


renderSortButton : String -> SortType -> SortType -> Html Msg
renderSortButton name sort currentSort =
    button
        [ onClick (SetPosition sort)
        , class
            (if currentSort == sort then
                "selected-sort"

             else
                ""
            )
        ]
        [ text name ]


getLink : List String -> String
getLink x =
    case List.head x of
        Just y ->
            y

        Nothing ->
            ""


singleOrMultiple : List String -> String -> List (Html Msg)
singleOrMultiple strs name =
    if List.length strs == 1 then
        [ a [ href (getLink strs) ] [ text name ] ]

    else
        List.indexedMap (\idx link -> a [ href link ] [ text (name ++ " " ++ String.fromInt (idx + 1) ++ ", ") ]) strs


renderRow : DataRow -> Html Msg
renderRow row =
    div [ class "entry flex" ]
        [ div [ class "flex-down meta-data" ]
            [ h3 [ class "headline" ] [ text row.headline ]
            , div [ class "flex edition" ]
                [ h5 [] [ text ("Edition " ++ row.edition ++ " Position " ++ String.fromInt row.position) ]
                ]
            , h5 [ class "links" ] (List.append (singleOrMultiple row.links "link") (singleOrMultiple row.hattips "source"))
            ]
        , p [] [ text row.text ]
        ]
