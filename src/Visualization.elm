module Visualization exposing (CsvRow, view)

import Axis
import Color exposing (Color)
import Dict exposing (Dict)
import Dict.Extra
import Html exposing (Html, text)
import List.Extra
import Path exposing (Path)
import Scale exposing (ContinuousScale, OrdinalScale)
import Scale.Color
import Shape exposing (StackResult)
import Time exposing (Month(..))
import Time.Extra exposing (Parts)
import TypedSvg exposing (g, svg, text_)
import TypedSvg.Attributes exposing (class, fill, fontFamily, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (fontSize)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..), Transform(..))


type alias CsvRow =
    { at : Time.Posix
    , artist : String
    , artistMbid : String
    , album : String
    , albumMbid : String
    , track : String
    , trackMbid : String
    }


type alias Config =
    { tz : Time.Zone
    , take : Int
    , proportional : Bool
    }


w : Float
w =
    1920 / 1.5


h : Float
h =
    1080 / 1.5


labelsWidth : Float
labelsWidth =
    100


padding : Float
padding =
    40


view : Config -> List CsvRow -> Html msg
view config rows =
    let
        { stacks, timeScale, colorScale } =
            analyze config rows
    in
    innerView stacks timeScale colorScale


analyze :
    Config
    -> List CsvRow
    ->
        { stacks : StackResult String
        , timeScale : ContinuousScale Time.Posix
        , colorScale : OrdinalScale String Color
        }
analyze { tz, take, proportional } rows =
    let
        cut : List CsvRow
        cut =
            rows
                |> Dict.Extra.groupBy (\{ artist } -> artist)
                |> Dict.values
                |> List.sortBy (\p -> -(List.length p))
                |> List.take take
                |> List.concat

        monthOf : Int -> ( Int, Month )
        monthOf timestamp =
            let
                posix : Time.Posix
                posix =
                    Time.millisToPosix timestamp
            in
            ( Time.toYear tz posix
            , Time.toMonth tz posix
            )

        ( ( firstYear, firstMonth ), ( lastYear, lastMonth ) ) =
            List.foldl
                (\{ at } ( mn, mx ) ->
                    let
                        timestamp : Int
                        timestamp =
                            Time.posixToMillis at
                    in
                    if mn == 0 then
                        ( timestamp, timestamp )

                    else
                        ( min mn timestamp, max mx timestamp )
                )
                ( 0, 0 )
                cut
                |> Tuple.mapBoth monthOf monthOf

        samples : List ( String, List Float )
        samples =
            cut
                |> Dict.Extra.groupBy
                    (\{ at } ->
                        ( Time.toYear tz at
                        , monthToInt (Time.toMonth tz at)
                        )
                    )
                |> Dict.toList
                |> List.concatMap
                    (\( month, data ) ->
                        let
                            grouped : List ( String, Int )
                            grouped =
                                data
                                    |> Dict.Extra.groupBy (\{ artist } -> artist)
                                    |> Dict.map (\_ t -> List.length t)
                                    |> Dict.toList

                            monthTotal : Int
                            monthTotal =
                                grouped
                                    |> List.map Tuple.second
                                    |> List.sum
                        in
                        List.map
                            (\( artist, count ) ->
                                ( artist
                                , ( month
                                  , if proportional then
                                        toFloat count * 100 / toFloat monthTotal

                                    else
                                        toFloat count
                                  )
                                )
                            )
                            grouped
                    )
                |> Dict.Extra.groupBy Tuple.first
                |> Dict.map
                    (\_ values ->
                        let
                            dict : Dict ( Int, Int ) Float
                            dict =
                                values
                                    |> List.map Tuple.second
                                    |> Dict.fromList
                        in
                        List.range firstYear lastYear
                            |> List.concatMap
                                (\year ->
                                    monthRange
                                        (if year == firstYear then
                                            firstMonth

                                         else
                                            Jan
                                        )
                                        (if year == lastYear then
                                            lastMonth

                                         else
                                            Dec
                                        )
                                        |> List.map
                                            (\month ->
                                                Dict.get ( year, monthToInt month ) dict
                                                    |> Maybe.withDefault 0
                                            )
                                )
                    )
                |> Dict.toList

        colorScale : OrdinalScale String Color
        colorScale =
            samples
                |> List.map Tuple.first
                |> List.reverse
                |> Scale.ordinal Scale.Color.category10

        timeScale : ContinuousScale Time.Posix
        timeScale =
            -- construct the time domain for display
            -- the data is per-month, so we have to pick a day
            -- to get the ticks to show up correctly, the upper bound needs to be Jan 2 (Jan 1 does not work).
            Scale.time
                Time.utc
                ( 0, w - padding * 2 - labelsWidth )
                ( fromCalendarDate ( firstYear, firstMonth ) 1, fromCalendarDate ( lastYear, lastMonth ) 2 )

        fromCalendarDate : ( Int, Month ) -> Int -> Time.Posix
        fromCalendarDate ( year, month ) day =
            Time.Extra.partsToPosix tz (Parts year month day 0 0 0 0)
    in
    { stacks =
        Shape.stack
            { data = samples
            , offset = Shape.stackOffsetNone
            , order = List.sortBy (Tuple.second >> List.sum >> negate)
            }
    , timeScale = timeScale
    , colorScale = colorScale
    }


monthRange : Month -> Month -> List Month
monthRange from to =
    List.range (monthToInt from) (monthToInt to)
        |> List.map intToMonth


intToMonth : Int -> Month
intToMonth month =
    case month of
        1 ->
            Jan

        2 ->
            Feb

        3 ->
            Mar

        4 ->
            Apr

        5 ->
            May

        6 ->
            Jun

        7 ->
            Jul

        8 ->
            Aug

        9 ->
            Sep

        10 ->
            Oct

        11 ->
            Nov

        _ ->
            Dec


monthToInt : Month -> Int
monthToInt month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


innerView : StackResult String -> ContinuousScale Time.Posix -> OrdinalScale String Color -> Svg msg
innerView { values, labels, extent } timeScale colorScale =
    let
        sampleColor : String -> Color
        sampleColor label =
            Scale.convert colorScale label |> Maybe.withDefault Color.black

        colors : List Color
        colors =
            List.map sampleColor labels

        size : Int
        size =
            List.head values
                |> Maybe.map List.length
                |> Maybe.withDefault 0

        xScale : ContinuousScale Float
        xScale =
            -- map an index to screen space
            Scale.linear ( padding, w - padding - labelsWidth ) ( 0, toFloat size - 1 )

        yScale : ContinuousScale Float
        yScale =
            Scale.linear ( h - padding, padding ) extent

        xAxis : Svg msg
        xAxis =
            timeScale
                |> Axis.bottom [ Axis.tickCount 1 ]

        paths =
            List.map2 (renderStream ( xScale, yScale )) colors values

        labelPositions =
            let
                position ys =
                    ys
                        |> List.Extra.last
                        |> Maybe.withDefault ( 0, 0 )
                        |> (\( y1, y2 ) -> (y2 + y1) / 2)
                        |> Scale.convert yScale
            in
            List.map position values

        labelElement : String -> Float -> Svg msg
        labelElement label yPosition =
            g [ transform [ Translate (w - padding - labelsWidth + 10) yPosition ] ]
                [ text_ [ fill <| Paint <| sampleColor label ] [ text label ] ]
    in
    svg [ viewBox 0 0 w h ]
        [ g [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ xAxis ]
        , g [ class [ "series" ] ] paths
        , g [ fontFamily [ "sans-serif" ], fontSize 10 ]
            (List.map2 labelElement labels labelPositions)
        ]


{-| Renders one colored stream with given scaling
-}
renderStream : ( ContinuousScale Float, ContinuousScale Float ) -> Color -> List ( Float, Float ) -> Svg msg
renderStream scales color coords =
    Path.element (toArea scales coords) [ fill (Paint color) ]


{-| Create a svg path string that draws the area between two lines
-}
toArea : ( ContinuousScale Float, ContinuousScale Float ) -> List ( Float, Float ) -> Path
toArea ( scaleX, scaleY ) ys =
    let
        mapper : Int -> ( Float, Float ) -> Maybe ( ( Float, Float ), ( Float, Float ) )
        mapper index ( y1, y2 ) =
            let
                xCoord =
                    index
                        |> toFloat
                        |> Scale.convert scaleX

                ( low, high ) =
                    if y1 < y2 then
                        ( y1, y2 )

                    else
                        ( y2, y1 )
            in
            Just
                ( ( xCoord, Scale.convert scaleY low )
                , ( xCoord, Scale.convert scaleY high )
                )
    in
    List.indexedMap mapper ys
        |> Shape.area Shape.monotoneInXCurve
