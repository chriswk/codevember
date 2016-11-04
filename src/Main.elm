module Main exposing (..)

import Html.App as App
import Html
import Html.Attributes as A
import Html.Events exposing (onClick, onInput)
import Color exposing (rgb, Color)
import Graphics.Render exposing (..)
import Random
import Random.Extra as RE
import Time exposing (Time)
import String


type alias Canvas =
    { width : Float, height : Float }


type alias Model =
    { shapes : List Shape
    , canvas : Canvas
    , started : Bool
    , maxWidth : Float
    , maxHeight : Float
    , minSide : Float
    , maxSide : Float
    }


type Msg
    = NoOp
    | GenerateShape Time
    | NewShape Shape
    | UpdateWidth String
    | UpdateHeight String
    | UpdateMinSide String
    | UpdateMaxSide String
    | Toggle
    | Clear


type Geometry
    = Square
    | Circle


type Colour
    = Red
    | Blue
    | Green
    | Yellow


type alias Shape =
    { geometry : Geometry
    , colour : Color.Color
    , side : Float
    , position : ( Float, Float )
    }


generatePosition : Model -> Random.Generator ( Float, Float )
generatePosition model =
    let
        splitX =
            model.canvas.width / 2

        splitY =
            model.canvas.height / 2

        minX =
            0 - splitX

        maxX =
            0 + splitX

        minY =
            0 - splitY

        maxY =
            0 + splitY
    in
        Random.pair (Random.float minX maxX) (Random.float minY maxY)


canvasSize : { width : Float, height : Float }
canvasSize =
    { width = 1280, height = 1024 }


generateRadius : Model -> Random.Generator Float
generateRadius model =
    Random.float model.minSide model.maxSide


generateShape : Model -> Random.Generator Shape
generateShape model =
    Random.map4 Shape generateGeometry generateColor (generateRadius model) (generatePosition model)


generateGeometry : Random.Generator Geometry
generateGeometry =
    RE.frequency
        [ ( 1, RE.constant Square )
        , ( 1, RE.constant Circle )
        ]


generateColor : Random.Generator Color
generateColor =
    RE.frequency
        [ ( 2, RE.constant Color.blue )
        , ( 1, RE.constant Color.red )
        , ( 1, RE.constant Color.green )
        , ( 1, RE.constant Color.yellow )
        ]


timeToSeed : Float -> Random.Seed
timeToSeed t =
    Random.initialSeed (round t)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        GenerateShape _ ->
            model ! [ Random.generate NewShape (generateShape model) ]

        NewShape shape ->
            { model | shapes = shape :: model.shapes } ! []

        UpdateWidth width ->
            let
                w =
                    min model.maxWidth

                newWidth =
                    Result.withDefault model.canvas.width (String.toFloat width)

                canvas =
                    model.canvas

                newCanvas =
                    { canvas | width = w newWidth }
            in
                { model | canvas = newCanvas } ! []

        UpdateHeight height ->
            let
                h =
                    min model.maxHeight

                newHeight =
                    Result.withDefault
                        model.canvas.height
                        (String.toFloat height)

                canvas =
                    model.canvas

                newCanvas =
                    { canvas | height = h newHeight }
            in
                { model | canvas = newCanvas } ! []

        UpdateMinSide s ->
            let
                newMinSide =
                    Result.withDefault model.minSide (String.toFloat s)
                        |> max 0
                        |> min model.maxSide
            in
                { model | minSide = newMinSide } ! []

        UpdateMaxSide s ->
            let
                newMaxSide =
                    Result.withDefault model.maxSide (String.toFloat s)
                        |> min model.canvas.height
                        |> max model.minSide
            in
                { model | maxSide = newMaxSide } ! []

        Toggle ->
            { model | started = (not model.started) } ! []

        Clear ->
            { model | shapes = [] } ! []


initialModel : Model
initialModel =
    { shapes = []
    , canvas = { width = 1280, height = 1024 }
    , started = False
    , maxHeight = 2140
    , maxWidth = 3860
    , minSide = 20
    , maxSide = 200
    }


init : ( Model, Cmd Msg )
init =
    initialModel ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    if (model.started) then
        Time.every (200 * Time.millisecond) GenerateShape
    else
        Sub.none


getColor : Colour -> Color.Color
getColor colour =
    case colour of
        Red ->
            rgb 255 0 0

        Blue ->
            rgb 0 255 0

        Green ->
            rgb 0 0 255

        Yellow ->
            rgb 255 255 0


drawSquare : Shape -> Form Msg
drawSquare s =
    rectangle s.side s.side
        |> solidFill s.colour


drawCircle : Shape -> Form Msg
drawCircle s =
    circle s.side
        |> solidFill s.colour


drawShape : Shape -> Form Msg
drawShape shape =
    let
        drawn =
            case shape.geometry of
                Square ->
                    drawSquare shape

                Circle ->
                    drawCircle shape
    in
        drawn
            |> position shape.position


view : Model -> Html.Html Msg
view model =
    let
        boundingBox =
            rectangle model.canvas.width model.canvas.height
                |> solidFill (rgb 15 15 15)

        shapesAsGroup =
            List.map drawShape model.shapes
                |> group
    in
        Html.div []
            [ Html.div []
                [ group
                    [ boundingBox
                    , shapesAsGroup
                    ]
                    |> svg model.canvas.width
                        model.canvas.height
                ]
            , menu model
            ]


formInput : String -> (String -> a) -> String -> Html.Html a
formInput label msg value =
    Html.div []
        [ Html.label [] [ Html.text label ]
        , Html.input [ onInput msg, A.value value ]
            []
        ]


menu : Model -> Html.Html Msg
menu model =
    let
        toggleText =
            if model.started then
                "Stop"
            else
                "Start"

        w =
            toString model.canvas.width

        h =
            toString model.canvas.height

        minS =
            toString model.minSide

        maxS =
            toString model.maxSide
    in
        Html.div []
            [ Html.button [ onClick Toggle ] [ Html.text toggleText ]
            , Html.button [ onClick Clear ] [ Html.text "Clear canvas" ]
            , formInput "Width" UpdateWidth w
            , formInput "Height" UpdateHeight h
            , formInput "Minimum Side" UpdateMinSide minS
            , formInput "Maximum Side" UpdateMaxSide maxS
            ]


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
