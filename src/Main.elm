module Main exposing (main)

import Browser
import Csv.Decode
import File exposing (File)
import File.Select
import Html
import Html.Attributes
import Html.Events
import Task
import Time
import Visualization exposing (CsvRow)


type Model
    = GettingTimezone
    | SelectingCsvFile Time.Zone
    | CsvDecodeError String
    | Loaded Time.Zone (List CsvRow)


type Msg
    = SelectFile Time.Zone
    | SelectedFile Time.Zone File
    | ReadFile Time.Zone String
    | GotTimezone Time.Zone


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( GettingTimezone, Time.here |> Task.perform GotTimezone )


view : Model -> Browser.Document Msg
view model =
    { title = "Spotify History Visualization"
    , body =
        case model of
            GettingTimezone ->
                [ Html.text "Loading..." ]

            SelectingCsvFile tz ->
                [ Html.p []
                    [ Html.text "Download your scrobbles in CSV from "
                    , Html.a [ Html.Attributes.href "https://lastfm.ghan.nl/export/" ] [ Html.text "lastfm.ghan.nl" ]
                    , Html.text " then "
                    , Html.button [ Html.Events.onClick (SelectFile tz) ] [ Html.text "Load your CSV file" ]
                    ]
                ]

            CsvDecodeError e ->
                [ Html.p [] [ Html.text ("There was an error reading your CSV file: " ++ e) ] ]

            Loaded tz rows ->
                [ Html.p [] [ Html.text ("Loaded " ++ String.fromInt (List.length rows) ++ " rows") ]
                , Visualization.view tz rows
                ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTimezone tz ->
            ( SelectingCsvFile tz, Cmd.none )

        SelectFile tz ->
            ( model, File.Select.file [ "text/csv" ] (SelectedFile tz) )

        SelectedFile tz file ->
            ( model, File.toString file |> Task.perform (ReadFile tz) )

        ReadFile tz content ->
            case Csv.Decode.decodeCsv Csv.Decode.FieldNamesFromFirstRow csvRowDecoder content of
                Err e ->
                    ( CsvDecodeError (Csv.Decode.errorToString e), Cmd.none )

                Ok result ->
                    ( Loaded tz result, Cmd.none )


csvRowDecoder : Csv.Decode.Decoder CsvRow
csvRowDecoder =
    Csv.Decode.succeed CsvRow
        |> Csv.Decode.pipeline (Csv.Decode.field "uts" timestampDecoder)
        |> Csv.Decode.pipeline (Csv.Decode.field "artist" Csv.Decode.string)
        |> Csv.Decode.pipeline (Csv.Decode.field "artist_mbid" Csv.Decode.string)
        |> Csv.Decode.pipeline (Csv.Decode.field "album" Csv.Decode.string)
        |> Csv.Decode.pipeline (Csv.Decode.field "album_mbid" Csv.Decode.string)
        |> Csv.Decode.pipeline (Csv.Decode.field "track" Csv.Decode.string)
        |> Csv.Decode.pipeline (Csv.Decode.field "track_mbid" Csv.Decode.string)


timestampDecoder : Csv.Decode.Decoder Time.Posix
timestampDecoder =
    intAsStringDecoder
        |> Csv.Decode.map
            (\stamp ->
                Time.millisToPosix (stamp * 1000)
            )


intAsStringDecoder : Csv.Decode.Decoder Int
intAsStringDecoder =
    Csv.Decode.string
        |> Csv.Decode.andThen
            (\str ->
                case String.toInt str of
                    Nothing ->
                        Csv.Decode.fail ("Could not parse " ++ str ++ " as int")

                    Just int ->
                        Csv.Decode.succeed int
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
