module Main exposing (..)

import Html
import Html.Attributes exposing (..)
import Html.App
import Time exposing (Time)
import Date exposing (..)
import String
import Date.Extra.Field exposing (..)
import Date.Extra.Duration exposing (..)
import Svg exposing (..)
import Html exposing (..)
import Svg.Attributes exposing (..)


type alias Model =
    { current : Maybe Date
    , endOfDay : Maybe Date
    }


type Msg
    = Tick Time


homeHour : Int
homeHour =
    17


homeMinute : Int
homeMinute =
    30


main : Program Never
main =
    Html.App.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    { current = Nothing
    , endOfDay = Nothing
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick timenow ->
            ( updateCurrentTimes model timenow
                |> setEndOfDay
            , Cmd.none
            )


updateCurrentTimes : Model -> Time -> Model
updateCurrentTimes model timenow =
    { model | current = Just (Date.fromTime timenow), endOfDay = Just (Date.fromTime timenow) }


setEndOfDay : Model -> Model
setEndOfDay model =
    case model.endOfDay of
        Just a ->
            { model | endOfDay = Just (getEndOfDay a) }

        Nothing ->
            model


getEndOfDay : Date -> Date
getEndOfDay date =
    getAlteredDate (Date.Extra.Field.Hour homeHour) date
        |> getAlteredDate (Date.Extra.Field.Minute homeMinute)
        |> getAlteredDate (Date.Extra.Field.Second 0)
        |> getAlteredDate (Date.Extra.Field.Millisecond 0)


getDiff : Date -> Date -> Date.Extra.Duration.DeltaRecord
getDiff date1 date2 =
    Date.Extra.Duration.diff date1 date2


getAlteredDate : Date.Extra.Field.Field -> Date -> Date
getAlteredDate field date =
    let
        newDate =
            (Date.Extra.Field.fieldToDate (field) date)
    in
        case newDate of
            Just a ->
                a

            Nothing ->
                date


timeTextFromDelta : Date.Extra.Duration.DeltaRecord -> String
timeTextFromDelta delta =
    String.concat
        [ toString (abs delta.hour)
        , " Hours "
        , (toString (abs delta.minute))
        , " Minutes "
        , (toString (abs delta.second))
        , " Seconds "
        ]


isHomeTime : Date.Extra.Duration.DeltaRecord -> Bool
isHomeTime delta =
    (delta.hour >= 0 && delta.minute >= 0 && delta.second >= 0)


view : Model -> Html.Html Msg
view model =
    containerDiv model


containerDiv : Model -> Html.Html Msg
containerDiv model =
    Html.div
        [ Html.Attributes.style
            [ ( "height", "100vh" )
            , ( "width", "100%" )
            ]
        ]
        [ contentDiv model ]


contentDiv : Model -> Html.Html Msg
contentDiv model =
    Html.div
        [ Html.Attributes.style
            [ ( "font-size", "100px" )
            , ( "font-family", "Roboto,Helvetica,Arial,sans-serif" )
            , ( "line-height", "1" )
            , ( "letter-spacing", "-.04em" )
            , ( "opacity", "0.54" )
            , ( "width", "100%" )
            , ( "text-align", "center" )
            , ( "position", "relative" )
            , ( "top", "10%" )
            ]
        ]
        (contentElements model)


contentElements : Model -> List (Html.Html Msg)
contentElements model =
    case model.current of
        Just a ->
            case model.endOfDay of
                Just b ->
                    if (isHomeTime (getDiff a b)) then
                        [ div [] [ Html.text "It's Home Time!!!" ], div [] [ homeSvg ], ("+ " ++ ((getDiff a b) |> timeTextFromDelta) |> Html.text) ]
                    else
                        [ (("- " ++ ((getDiff a b) |> timeTextFromDelta)) |> Html.text) ]

                Nothing ->
                    []

        Nothing ->
            []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Time.every Time.second Tick ]


homeSvg : Html.Html Msg
homeSvg =
    Html.div []
        [ Svg.svg [ attribute "height" "350px", viewBox "0 0 500 500", attribute "width" "500px", attribute "xmlns" "http://www.w3.org/2000/svg", attribute "xmlns:xlink" "http://www.w3.org/1999/xlink" ]
            [ Svg.path [ d "M76.403,450.517c0,0-0.412,10.62,9.967,10.62c12.923,0,119.849-0.138,119.849-0.138l0.189-98.212c0,0-1.701-16.188,14.023-16.188h49.733c18.577,0,17.426,16.188,17.426,16.188l-0.206,97.903c0,0,101.392,0,117.322,0c13.181,0,12.579-13.232,12.579-13.232V266.396L251.948,119.31L76.403,266.396V450.517z", fill "#010101", Svg.Attributes.id "path-0" ]
                []
            , Svg.path [ d "M11.753,252.7c0,0,14.917,27.479,47.414,0L253.409,88.377l182.109,163.309c37.636,27.136,51.728,0,51.728,0L253.409,39.863L11.753,252.7z", fill "#010101", Svg.Attributes.id "path-1", attribute "style" "position: relative;" ]
                []
            , Svg.node "polygon"
                [ fill "#010101", attribute "points" "431.103,87.981 384.256,87.981 384.462,144.795 431.103,184.372 " ]
                []
            , Svg.node "circle"
                [ attribute "cx" "245.923", attribute "cy" "305.658", fill "#FFFFFF", Svg.Attributes.id "circle-0", attribute "r" "25.641", attribute "style" "position: relative;" ]
                []
            ]
        ]
