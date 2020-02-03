module Main exposing (main)

import Angle
import BoundingBox2d exposing (BoundingBox2d)
import Browser
import Browser.Dom
import Browser.Events
import Circle2d
import Geometry.Svg as Svg
import Html exposing (Html)
import LineSegment2d
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Poisson
import Quantity exposing (Quantity)
import Random
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Task

type YUpCoordinates = YUpCoordinates

type alias Model =
    { window : BoundingBox2d Pixels YUpCoordinates
    , points : List (Point2d Pixels YUpCoordinates)
    }


type Msg
    = GotViewport Browser.Dom.Viewport
    | WindowResize Float Float
    | RandomPoint (List (Point2d Pixels YUpCoordinates))


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , view = view
        , update = update
        }


init : () -> ( Model, Cmd Msg )
init _ =
    (
        { window = BoundingBox2d.from Point2d.origin Point2d.origin
        , points = []
        }
    , Task.perform GotViewport Browser.Dom.getViewport
    )


subscriptions : model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\w h -> WindowResize (toFloat w) (toFloat h))


view : Model -> Html Msg
view model =
    let
        pixelsAsString x =
            String.fromInt <| round <| Pixels.inPixels x

        points =
            model.points
                |> List.map drawCircle
    in
    Svg.svg
        [ Attributes.width <| pixelsAsString (BoundingBox2d.maxX model.window)
        , Attributes.height <| pixelsAsString (BoundingBox2d.maxY model.window)
        ]
        points


drawCircle : Point2d Pixels YUpCoordinates -> Svg msg
drawCircle center =
    Svg.circle2d
        [ Attributes.fill "black" ]
        (Circle2d.withRadius (Pixels.pixels 10) center)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        boundingBox width height =
            BoundingBox2d.from Point2d.origin (Point2d.xy width height)

        bboxFromViewport viewport =
            boundingBox (Pixels.pixels viewport.width) (Pixels.pixels viewport.height)
    in
    case msg of
        GotViewport { scene, viewport } ->
            ( { model | window = bboxFromViewport viewport }
            , bboxFromViewport viewport
                |> Poisson.sample 30
                |> Random.generate RandomPoint
            )

        WindowResize width height ->
            ( { model | window = boundingBox (Pixels.pixels width) (Pixels.pixels height) }
            , Cmd.none
            )

        RandomPoint points ->
            ( { model | points = points }
            , Cmd.none
            )
