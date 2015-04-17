module Geometry where

type alias V2 =
    { x : Float
    , y : Float
    }

-- Set of points (x, y) with a*x + b*y = c
type alias Line2 =
    { a : Float
    , b : Float
    , c : Float
    }

type alias Ray2 =
    { v0 : V2
    , dv : V2
    }

type alias Seg2 =
    { v0 : V2
    , v1 : V2
    }

intersection : Line2 -> Line2 -> Maybe V2
intersection l1 l2 =
    let det = l1.a * l2.b - l1.b * l2.a
    in
        if det == 0 
        then Nothing
        else Just
            { x = (l2.b * l1.c - l1.b * l2.c) / det
            , y = (l1.a * l2.c - l2.a * l1.c) / det
            }

lineNormal : Line2 -> V2
lineNormal l = { x = l.a, y = l.b }

infixr 7 .*
(.*) : Float -> V2 -> V2
c .* v = { x = c * v.x, y = c * v.y }

infixr 6 .+
(.+) : V2 -> V2 -> V2
v1 .+ v2 = { x = v1.x + v2.x, y = v1.y + v2.y }

evalRay : Ray2 -> Float -> V2
evalRay ray t = ray.v0 .+ t .* ray.dv

{-
if our ray is vt = v0 + t * dv
Then we're solving

l.a * vt.x + l.b * vt.y = l.c
l.a * (v0.x + t * dv.x) + (l.b * v0.y + t * dv.y) = l.c
l.a * v0.x + l.b * v0.y + (l.a * dv.x + l.b * dv.y) * t = l.c
(l.a * dv.x + l.b * dv.y) * t = l.c - l.a * v0.x - l.b * v0.y
t = (l.c - l.a * v0.x - l.b * v0.y) / (l.a * dv.x + l.b * dv.y)
-}
rayLineIntersection : Ray2 -> Line2 -> Maybe Float
rayLineIntersection r l =
    if l.a * r.dv.x + l.b * r.dv.y == 0
    then Nothing
    else Just ((l.c - l.a * r.v0.x - l.b * r.v0.y) / (l.a * r.dv.x + l.b * r.dv.y))

toV2 : Float -> Float -> V2
toV2 x y = { x = x, y = y }

fromV2 : V2 -> (Float, Float)
fromV2 v = (v.x, v.y)

segLine : V2 -> V2 -> Line2
segLine v0 v1 =
    let dx = v1.x - v0.x
        dy = v1.y - v0.y
    in
    { a = -dy, b = dx, c = dx * v0.y - dy * v0.x }

-- This checks if v is on or inside the circle with diameter v0-v1.
-- We use it to check whether v is between v0 and v1 assuming it's on
-- the line defined by those two points, so we are away from the
-- boundary condition unless we're right at one of the endpoints.
between : V2 -> V2 -> V2 -> Bool
between v0 v1 v =
    (v0.x - v.x) * (v1.x - v.x) + (v0.y - v.y) * (v1.y - v.y) <= 0

unitVec : Float -> V2
unitVec theta = { x = cos theta, y = sin theta }
