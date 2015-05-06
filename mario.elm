import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window
import Debug
import List
import Mouse

-- MODEL
type alias Mario = { x:Float, y:Float, vx:Float, vy:Float, dir:String }
type alias Marios = List Mario
type alias ArrowKeys = { x:Int, y:Int }
mario1 = { x=-10, y=100, vx=1, vy=0, dir="right" }
mario2 = { x=-15, y=103, vx=-1, vy=0, dir="left" }

marios = [mario1, mario2]

type alias State = Marios

-- UPDATE -- ("m" is for Mario)
bounceVelocity = 10
xVelocity = 5

jump {y} m = if y > 0 && m.y == 0 then { m | vy <- 5 } else m
gravity t m = if m.y > 0 then { m | vy <- m.vy - t/4 } else { m | vy <- bounceVelocity }
physics t m = { m | x <- m.x + t*m.vx ,
                    y <- max 0 (m.y + t*m.vy) }
walk {x} m = { m | vx <- if x /=0 then (toFloat x*xVelocity) else m.vx
                 , dir <- if x < 0 then "left" else
                          if x > 0 then "right" else m.dir }

-- { m | vx <- if (abs m.x/4) > w then -3 } -- -m.vx else m.vx }
bounds : Int -> Int -> Mario -> Mario
bounds w h m =
  let (vx, dir) = if abs(m.x) > toFloat(w)/2
                  then (-m.vx, if m.dir == "right" then "left" else "right")
                  else (m.vx, m.dir)
  in { m | vx <- vx, dir <- dir }

steps : (Time, ArrowKeys, (Int, Int)) -> Marios -> Marios
steps (dt, keys, (w, h)) marios =
  List.map (step (dt, keys, (w, h))) marios

step : (Time, ArrowKeys, (Int, Int)) -> Mario -> Mario
step (dt, keys, (width, height)) mario =
  mario
   -- |> jump keys
   |> gravity dt
   -- |> walk keys
   |> physics dt
   |> bounds width height
   |> Debug.watch "mario"


-- DISPLAY

marioImage: Float -> Mario -> Form
marioImage height mario =
  let verb = if | mario.y  >  0 -> "jump"
                | mario.vx /= 0 -> "walk"
                | otherwise     -> "stand"
      src = "/imgs/mario/" ++ verb ++ "/" ++ mario.dir ++ ".gif"
  in (image 35 35 src)
        |> toForm
        |> Debug.trace "mario"
        |> move (mario.x, mario.y + 62 - height/2)


renders: (Int, Int) -> Marios -> Bool -> Element
renders (w,h) marios mouseDown =
  let (w',h') = (toFloat w, toFloat h)
  in collage w h
     (
      (rect w' h'  |> filled (rgb 174 238 238)) ::
      (rect w' 50 |> filled (rgb 74 163 41)
                 |> move (0, 24 - h'/2)) ::
      (List.map (marioImage h') marios)
    )

-- MARIO
-- fps : number -> Signal Time
-- arrows : Signal { x : Int, y : Int }
input : Signal (Time, ArrowKeys, (Int, Int))
input = let delta = Signal.map (\t -> t/20) (fps 25)
        in  Signal.sampleOn delta (Signal.map3 (,,) delta Keyboard.arrows Window.dimensions)

-- foldp : (a -> state -> state) -> state -> Signal a -> Signal state
-- map2  : (a -> b -> result) -> Signal a -> Signal b -> Signal result
-- map   : (a -> result) -> Signal a -> Signal result
-- (<~)  : (a -> b)      -> Signal a -> Signal b
-- (~)   : Signal (a -> b) -> Signal a -> Signal b

main : Signal.Signal Element
main = Signal.map3 renders
        Window.dimensions
        (Signal.foldp steps marios input)
        Mouse.isDown
