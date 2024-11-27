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
    | Loaded
        { tz : Time.Zone
        , count : String
        , rows : List CsvRow
        , proportional : Bool
        }


type Msg
    = SelectFile Time.Zone
    | SelectedFile Time.Zone File
    | ReadFile Time.Zone String
    | GotTimezone Time.Zone
    | Count String
    | Proportional Bool


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

            Loaded { tz, count, rows, proportional } ->
                [ Html.p []
                    [ Html.text
                        "Show top "
                    , Html.input
                        [ Html.Events.onInput Count
                        , Html.Attributes.value count
                        , Html.Attributes.type_ "number"
                        , Html.Attributes.min "1"
                        ]
                        []
                    ]
                , Html.label []
                    [ Html.text
                        "Show proportional "
                    , Html.input
                        [ Html.Events.onCheck Proportional
                        , Html.Attributes.checked proportional
                        , Html.Attributes.type_ "checkbox"
                        ]
                        []
                    ]
                , case String.toInt count of
                    Nothing ->
                        Html.text "Invalid number"

                    Just take ->
                        Visualization.view
                            { tz = tz
                            , take = take
                            , proportional = proportional
                            }
                            rows
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

                Ok rows ->
                    ( Loaded
                        { tz = tz
                        , count = "10"
                        , rows = rows
                        , proportional = True
                        }
                    , Cmd.none
                    )

        Count count ->
            case model of
                Loaded loaded ->
                    ( Loaded { loaded | count = count }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Proportional proportional ->
            case model of
                Loaded loaded ->
                    ( Loaded { loaded | proportional = proportional }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


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
