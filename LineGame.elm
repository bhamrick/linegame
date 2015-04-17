import Color (..)
import Debug
import Geometry (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import Keyboard
import List
import Signal
import Signal (Signal, (<~), (~))
import Time (..)
import Window

type alias LineData =
    { x : Float
    , y : Float
    , angle : Float
    , length : Float
    , motion : Movement
    }

type Movement = Forward Float | Rotating Float

type alias Wall =
    { x1 : Float
    , y1 : Float
    , x2 : Float
    , y2 : Float
    }

simpleMap : List Wall
simpleMap = [ { x1 = -200, y1 = -200, x2 = 200, y2 = -200 }
            , { x1 = 200, y1 = -200, x2 = 200, y2 = 200 }
            , { x1 = 200, y1 = 200, x2 = -200, y2 = 200 }
            , { x1 = -200, y1 = 200, x2 = -200, y2 = -200 }
            ]

rayWallIntersection : Ray2 -> Wall -> Maybe Float
rayWallIntersection r w =
    let v1 = toV2 w.x1 w.y1
        v2 = toV2 w.x2 w.y2
        l = segLine v1 v2
    in
    case rayLineIntersection r l of
        Just t -> if t >= 0 && between v1 v2 (evalRay r t)
            then Just t
            else Nothing
        Nothing -> Nothing

minMaybe : Maybe Float -> Maybe Float -> Maybe Float
minMaybe f1 f2 = case f1 of
    Just x -> case f2 of
        Just y -> Just (min x y)
        Nothing -> Just x
    Nothing -> f2

nextCollisionForRay : Ray2 -> List Wall -> Maybe Float
nextCollisionForRay r ws = List.foldr minMaybe Nothing (List.map (rayWallIntersection r) ws)

playerRay : LineData -> Maybe Ray2
playerRay line = case line.motion of
    Forward vel ->
        Just { v0 = toV2 (line.x + line.length * cos line.angle) (line.y + line.length * sin line.angle)
             , dv = toV2 (vel * cos line.angle) (vel * sin line.angle)
             }
    _ -> Nothing

nextCollision : LineData -> List Wall -> Maybe Float
nextCollision line walls =
    case playerRay line of
        Nothing -> Nothing
        Just r -> nextCollisionForRay r walls

type alias GameData =
    { walls : List Wall
    , player : LineData
    }

moveLine : Float -> LineData -> LineData
moveLine dt line = case line.motion of
    Forward v ->
        let dx = v * dt * cos line.angle
            dy = v * dt * sin line.angle
        in { line | x <- line.x + dx, y <- line.y + dy }
    Rotating w -> { line | angle <- normalizeAngle (line.angle + w * dt) }

flipLine : LineData -> LineData
flipLine line =
    { line
    | x <- line.x + line.length * cos line.angle
    , y <- line.y + line.length * sin line.angle
    , angle <- normalizeAngle (line.angle + pi)
    }

handleInput : Bool -> LineData -> LineData
handleInput spaceDown line = if spaceDown
    then case line.motion of
        Forward _ -> line
        Rotating _ -> { line | motion <- Forward 200 }
    else line

handleCollision : LineData -> LineData
handleCollision line =
    { line
    | motion <- Rotating (2 * pi)
    } |> flipLine

updateGame : (Time, Bool) -> GameData -> GameData
updateGame (dt, spaceDown) game =
    case Debug.watch "collision" (nextCollision game.player game.walls) of
        Nothing ->
            { game
            | player <- game.player
                |> moveLine dt
                |> handleInput spaceDown
                |> Debug.watch "player"
            }
        Just dt' ->
            if Debug.watch "dt gap" (dt' - dt) >= 0
            then
                { game
                | player <- game.player
                    |> moveLine dt
                    |> handleInput spaceDown
                    |> Debug.watch "player"
                }
            else
                { game
                | player <- game.player
                    |> moveLine dt'
                    |> handleCollision
                    |> moveLine (dt - dt')
                    |> Debug.watch "player"
                }

evolvedGame : GameData -> Signal GameData
evolvedGame startState = Signal.foldp updateGame startState input

input : Signal (Time, Bool)
input = Signal.sampleOn clock (Signal.map2 (,) clock Keyboard.space)

simpleGame : GameData
simpleGame =
    { walls = simpleMap
    , player =
        { x = 0
        , y = 0
        , angle = pi/6
        , length = 50
        , motion = Rotating (2 * pi)
        }
    }

render : (Int, Int) -> (Float, Float) -> GameData -> Element
render (w', h') (x0, y0) game =
    let w = toFloat w'
        h = toFloat h'
        wallForms = List.map wallForm game.walls
        playerForm = traced playerLine (lineSegment game.player)
        forms = playerForm :: wallForms
    in
    collage w' h' forms

wallForm : Wall -> Form
wallForm w = traced wallLine (segment (w.x1, w.y1) (w.x2, w.y2))

main : Signal Element
main = render <~ Window.dimensions ~ Signal.constant (0, 0) ~ evolvedGame simpleGame

fps' : Float -> Signal Time
fps' n = Signal.map (\x -> x / 1000) (fps n)

clock = fps' 60

floatMod : Float -> Float -> Float
floatMod x m = x - m * toFloat (floor (x / m))

normalizeAngle : Float -> Float
normalizeAngle theta = theta `floatMod` (2 * pi)

lineSegment : LineData -> Path
lineSegment l = segment (l.x, l.y) (l.x + l.length * (cos l.angle), l.y + l.length * (sin l.angle))

playerLine : LineStyle
playerLine =
    { defaultLine |
        color <- green,
        width <- 8,
        cap <- Round
    }

wallLine : LineStyle
wallLine = solid blue
