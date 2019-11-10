module Main exposing
    ( Model(..)
    , Msg(..)
    , Train
    , TrainTable
    , colorizeName
    , formatNotes
    , formatTime
    , getTimetable
    , init
    , main
    , renderTable
    , sanitizeName
    , subscriptions
    , timetableDecoder
    , toRow
    , update
    , view
    , viewTimetable
    )

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Train =
    { abbreviation : String
    , number : Maybe String
    , departureTime : String
    , destination : String
    , track : String
    , notes : Maybe String
    }


type alias TrainTable =
    List Train


type Model
    = Failure String
    | Loading
    | Success TrainTable


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getTimetable )



-- UPDATE


type Msg
    = GotTimetable (Result Http.Error TrainTable)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTimetable result ->
            case result of
                Ok traintable ->
                    ( Success traintable, Cmd.none )

                Err err ->
                    ( Failure (Debug.toString err), Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewTimetable model
        ]


viewTimetable : Model -> Html Msg
viewTimetable model =
    case model of
        Failure err ->
            div []
                [ text ("Err " ++ err) ]

        Loading ->
            div [] [ text "Loading..." ]

        Success traintable ->
            div [] [ renderTable traintable ]


renderTable : List Train -> Html msg
renderTable lst =
    table []
        ([ thead
            []
            [ th [] []
            , th [] []
            , th [] [ text "Nach" ]
            , th [] [ text "Gleis" ]
            , th [] [ text "Hinweis" ]
            ]
         ]
            ++ List.map toRow lst
        )


toRow : Train -> Html msg
toRow t =
    tr []
        [ td [ class "number" ]
            [ colorizeName
                (sanitizeName t.abbreviation t.number)
            ]
        , td [ class "time" ]
            [ formatTime t.departureTime
            ]
        , td [ class "destination" ]
            [ text t.destination
            ]
        , td [ class "track" ]
            [ formatTrack t.track
            ]
        , td [ class "notes" ]
            [ formatNotes t.notes
            ]
        ]


formatTime : String -> Html msg
formatTime string =
    text (String.slice 11 16 string)


colorizeName : String -> Html msg
colorizeName string =
    if
        String.startsWith "IC" string
            || String.startsWith "IR" string
            || String.startsWith "EC" string
    then
        div [ class "ic" ] [ text string ]

    else if String.startsWith "R" string then
        div [ class "r" ] [ text string ]

    else if String.startsWith "S" string then
        div [ class "s" ] [ text string ]

    else
        text string


sanitizeName : String -> Maybe String -> String
sanitizeName abbreviation maybeNumber =
    case maybeNumber of
        Just number ->
            abbreviation ++ " " ++ number

        Nothing ->
            abbreviation


formatTrack : String -> Html msg
formatTrack track =
    if String.endsWith "!" track then
        div [ class "yellow" ] [ text (String.slice 0 -1 track) ]

    else
        text track


formatNotes : Maybe String -> Html msg
formatNotes maybeNotes =
    case maybeNotes of
        Just notes ->
            if notes == "X" then
                text "Ausfall"

            else if notes == "+0" then
                text ""

            else if String.startsWith "+" notes then
                text
                    ("ca. "
                        ++ String.slice 1 (String.length notes) notes
                        ++ " Min später"
                    )

            else
                text notes

        Nothing ->
            text ""



-- HTTP


getTimetable : Cmd Msg
getTimetable =
    Http.get
        { url =
            "https://timetable.search.ch/api/stationboard.json"
                ++ "?stop=Olten"
                ++ "&show_delays=1"
                ++ "&show_trackchanges=1"
                ++ "&show_tracks=1&limit=16"
        , expect = Http.expectJson GotTimetable timetableDecoder
        }


timetableDecoder : Json.Decode.Decoder TrainTable
timetableDecoder =
    Json.Decode.field "connections"
        (Json.Decode.list
            (Json.Decode.map6 Train
                (Json.Decode.field "*G" Json.Decode.string)
                (Json.Decode.maybe
                    (Json.Decode.field "*L" Json.Decode.string)
                )
                (Json.Decode.field "time" Json.Decode.string)
                (Json.Decode.field "terminal"
                    (Json.Decode.field "name" Json.Decode.string)
                )
                (Json.Decode.field "track" Json.Decode.string)
                (Json.Decode.maybe
                    (Json.Decode.field "dep_delay" Json.Decode.string)
                )
            )
        )
