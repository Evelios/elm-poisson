module Poisson exposing (sample)

import BoundingBox2d exposing (BoundingBox2d)
import Point2d exposing (Point2d)


sample : Int -> BoundingBox2d unit cord -> List (Point2d unit cord)
sample num bbox =
    [ Point2d.origin ]
