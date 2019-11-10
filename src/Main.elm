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
    { departureTime : String
    , delay : Maybe String
    , abbreviation : String
    , destination : String
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
            div [] [ renderList traintable ]


renderList : List Train -> Html msg
renderList lst =
    ul []
        (List.map (\l -> li [] [ text (l.departureTime ++ ", " ++ formatDelay l.delay ++ ", " ++ l.abbreviation ++ ", " ++ l.destination) ]) lst)


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
        { url = "https://timetable.search.ch/api/stationboard.json?stop=Olten&show_delays=1&limit=10"
        , expect = Http.expectJson GotTimetable timetableDecoder
        }


timetableDecoder : Json.Decode.Decoder TrainTable
timetableDecoder =
    Json.Decode.field "connections"
        (Json.Decode.list
            (Json.Decode.map4 Train
                (Json.Decode.field "time" Json.Decode.string)
                (Json.Decode.maybe (Json.Decode.field "dep_delay" Json.Decode.string))
                (Json.Decode.field "number" Json.Decode.string)
                (Json.Decode.field "terminal" (Json.Decode.field "name" Json.Decode.string))
            )
        )
