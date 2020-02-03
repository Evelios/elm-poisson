module Poisson exposing (sample, randomPoint2d)

import BoundingBox2d exposing (BoundingBox2d)
import Point2d exposing (Point2d)
import Random exposing (Generator)
import Pixels exposing (Pixels)

sample : Int -> BoundingBox2d Pixels cord -> Generator (List (Point2d Pixels cord))
sample num bbox =
    let
        numAttempts = 30
    in
    Random.list numAttempts <| randomPoint2d bbox

randomPoint2d : BoundingBox2d Pixels coordinates -> Generator (Point2d Pixels coordinates)
randomPoint2d bbox =
    let
        randLength min max = Random.float (Pixels.inPixels min) (Pixels.inPixels max)
        randX = randLength (BoundingBox2d.minX bbox) (BoundingBox2d.maxX bbox)
        randY = randLength (BoundingBox2d.minY bbox) (BoundingBox2d.maxY bbox)
    in
    Random.map2 (\x y -> Point2d.xy (Pixels.pixels x) (Pixels.pixels y)) randX randY
