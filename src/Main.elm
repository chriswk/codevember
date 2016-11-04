module Main exposing (..)

import Html.App as App
import Html
import Color exposing (rgb, Color)
import Graphics.Render exposing (..)
import Random
import Random.Extra as RE
import Time exposing (Time)


type alias Model =
    { shapes : List Shape
    , seed : Maybe Random.Seed
    , maxShapes : Int
    }


type Msg
    = NoOp
    | GenerateShape Time
    | NewShape Shape
    | Tick Time


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
    , radius : Float
    , position : ( Float, Float )
    }


generatePosition : Random.Generator ( Float, Float )
generatePosition =
    let
        splitX =
            canvasSize.width / 2

        splitY =
            canvasSize.height / 2

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


generateRadius : Random.Generator Float
generateRadius =
    Random.float 20 150


generateShape : Random.Generator Shape
generateShape =
    Random.map4 Shape generateGeometry generateColor generateRadius generatePosition


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
            model ! [ Random.generate NewShape generateShape ]

        NewShape shape ->
            { model | shapes = shape :: model.shapes } ! []

        Tick t ->
            { model | seed = Just (timeToSeed t) } ! []


initialModel : Model
initialModel =
    { shapes = [], seed = Nothing, maxShapes = 50 }


init : ( Model, Cmd Msg )
init =
    initialModel ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    if (List.length model.shapes < model.maxShapes) then
        Time.every Time.second GenerateShape
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
    rectangle s.radius s.radius
        |> solidFill s.colour


drawCircle : Shape -> Form Msg
drawCircle s =
    circle s.radius
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
            rectangle canvasSize.width canvasSize.height
                |> solidFill (rgb 15 15 15)

        shapesAsGroup =
            List.map drawShape model.shapes
                |> group

        mostRecentShape =
            case List.head model.shapes of
                Nothing ->
                    Html.text ""

                Just s ->
                    Html.text (toString s)
    in
        Html.div []
            [ group
                [ boundingBox
                , shapesAsGroup
                ]
                |> svg canvasSize.width canvasSize.height
            , mostRecentShape
            ]


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
