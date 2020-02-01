module Main exposing (main)

import Angle
import BoundingBox2d
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
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Task


type alias Model =
    { width : Quantity Float Pixels
    , height : Quantity Float Pixels
    }


type Msg
    = GotViewport Browser.Dom.Viewport
    | WindowResize Float Float


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , view = view
        , update = update
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { width = Pixels.pixels 0
      , height = Pixels.pixels 0
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

        boundingBox =
            BoundingBox2d.from Point2d.origin (Point2d.xy model.width model.height)

        points =
            Poisson.sample 10 boundingBox
                |> List.map drawCircle
    in
    Svg.svg
        [ Attributes.width <| pixelsAsString model.width
        , Attributes.height <| pixelsAsString model.height
        ]
        points


drawCircle : Point2d Pixels Float -> Svg msg
drawCircle center =
    Svg.circle2d
        [ Attributes.fill "black" ]
        (Circle2d.withRadius (Pixels.pixels 10) center)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotViewport { scene, viewport } ->
            ( { model
                | width = Pixels.pixels viewport.width
                , height = Pixels.pixels viewport.height
              }
            , Cmd.none
            )

        WindowResize width height ->
            ( { model
                | width = Pixels.pixels width
                , height = Pixels.pixels height
              }
            , Cmd.none
            )
