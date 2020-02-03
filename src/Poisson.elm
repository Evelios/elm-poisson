module Poisson exposing (randomPoint2d, sample)

import Angle
import BoundingBox2d exposing (BoundingBox2d)
import Debug
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity)
import Random exposing (Generator)
import Vector2d


sample :
    Int
    -> Quantity Float Pixels
    -> BoundingBox2d Pixels cord
    -> Generator (List (Point2d Pixels cord))
sample num minDist bbox =
    let
        numAttempts =
            30

        generate ( inactive, active ) =
            case active of
                [] ->
                    Random.constant ( inactive, [] )

                test :: remaining ->
                    newCloud test ( test :: inactive, remaining )
                        |> Random.andThen generate

        newCloud around current =
            randomAround minDist (Quantity.twice minDist) around
                |> Random.list numAttempts
                |> Random.andThen (addPossibilities current)

        addPossibilities ( inactive, active ) possibilities =
            case possibilities of
                [] ->
                    Random.constant ( inactive, active )

                test :: remaining ->
                    if acceptablePoint (List.append inactive active) test then
                        addPossibilities ( inactive, test :: active ) remaining

                    else
                        addPossibilities ( inactive, active ) remaining

        acceptablePoint list point =
            hasSpacing list point && BoundingBox2d.contains point bbox

        hasSpacing list point =
            minDistanceFrom list point
                |> Maybe.map (Quantity.greaterThan minDist)
                |> Maybe.withDefault True
    in
    randomPoint2d bbox
        |> Random.andThen (\seed -> generate ( [], [ seed ] ))
        |> Random.map Tuple.first


minDistanceFrom : List (Point2d unit cord) -> Point2d unit cord -> Maybe (Quantity Float unit)
minDistanceFrom points reference =
    points
        |> List.map (Point2d.distanceFrom reference)
        |> Quantity.minimum


randomPoint2d : BoundingBox2d Pixels cord -> Generator (Point2d Pixels cord)
randomPoint2d bbox =
    let
        randLength min max =
            Random.float (Pixels.inPixels min) (Pixels.inPixels max)

        randX =
            randLength (BoundingBox2d.minX bbox) (BoundingBox2d.maxX bbox)

        randY =
            randLength (BoundingBox2d.minY bbox) (BoundingBox2d.maxY bbox)

        asPoint x y =
            Point2d.xy (Pixels.pixels x) (Pixels.pixels y)
    in
    Random.map2 asPoint randX randY


randomAround :
    Quantity Float Pixels
    -> Quantity Float Pixels
    -> Point2d Pixels cord
    -> Generator (Point2d Pixels cord)
randomAround min max point =
    let
        radius =
            Random.float (Pixels.inPixels min) (Pixels.inPixels max)

        theta =
            Random.float 0.0 (2.0 * pi)

        translated r a =
            Point2d.translateBy (Vector2d.rTheta (Pixels.pixels r) (Angle.radians a)) point
    in
    Random.map2 translated radius theta
