module Main exposing (Model(..), Msg(..), getTimetable, init, main, subscriptions, timetableDecoder, update, view, viewTimetable)

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
    , number : String
    , departureTime : String
    , destination : String
    , track : String
    , delay : Maybe String
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
        [ td [ class "number" ] [ text (formatName t.abbreviation t.number) ]
        , td [ class "time" ] [ text (formatTime t.departureTime) ]
        , td [ class "destination" ] [ text t.destination ]
        , td [ class "track" ] [ text t.track ]
        , td [ class "delay" ] [ text (formatDelay t.delay) ]
        ]


formatTime : String -> String
formatTime string =
    String.slice 11 16 string


formatName : String -> String -> String
formatName abbreviation number =
    -- Sometimes the abbreviation comes with the number
    if String.startsWith abbreviation number then
        number

    else
        abbreviation ++ " " ++ number


formatDelay : Maybe String -> String
formatDelay maybeDelay =
    case maybeDelay of
        Just delay ->
            delay

        Nothing ->
            ""



-- HTTP


getTimetable : Cmd Msg
getTimetable =
    Http.get
        { url = "http://transport.opendata.ch/v1/stationboard?id=Olten&limit=15"
        , expect = Http.expectJson GotTimetable timetableDecoder
        }


timetableDecoder : Json.Decode.Decoder TrainTable
timetableDecoder =
    Json.Decode.field "stationboard"
        (Json.Decode.list
            (Json.Decode.map6 Train
                (Json.Decode.field "category" Json.Decode.string)
                (Json.Decode.field "number" Json.Decode.string)
                (Json.Decode.field "stop" (Json.Decode.field "departure" Json.Decode.string))
                (Json.Decode.field "to" Json.Decode.string)
                (Json.Decode.field "stop" (Json.Decode.field "platform" Json.Decode.string))
                (Json.Decode.maybe (Json.Decode.field "stop" (Json.Decode.field "delay" Json.Decode.string)))
            )
        )
