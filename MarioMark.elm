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

mario0 = { x=-10, y=100, vx=xVelocity, vy=0, dir="right" }

type alias State = Marios

-- UPDATE -- ("m" is for Mario)

bounceVelocity = 15
xVelocity = 8
g = 5 -- gravity

jump {y} m = if y > 0 && m.y == 0 then { m | vy <- 5 } else m
gravity t m = if m.y > 0 then { m | vy <- m.vy - t/g} else { m | vy <- bounceVelocity }
physics t m = { m | x <- m.x + t*m.vx ,
                    y <- max 0 (m.y + t*m.vy) }
walk {x} m = { m | vx <- if x /=0 then (toFloat x*xVelocity) else m.vx
                 , dir <- if x < 0 then "left" else
                          if x > 0 then "right" else m.dir }

bounds : Int -> Int -> Mario -> Mario
bounds w h m =
  let (vx, dir) = if abs(m.x) > toFloat(w)/2
                  then (-m.vx, if m.dir == "right" then "left" else "right")
                  else (m.vx, m.dir)
  in { m | vx <- vx, dir <- dir }

update : (Time, ArrowKeys, (Int, Int), Bool) -> Marios -> Marios
update (dt, keys, (w, h), addMario) marios =
  let marios' = if addMario then mario0 :: marios else marios
      update' = (step (dt, keys, (w, h)))
      _ = {marioCount= List.length marios' }
            |> Debug.watch "Mario Count: "
      in List.map update' marios'

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


renders: (Int, Int) -> Marios -> Element
renders (w,h) marios =
  let (w',h') = (toFloat w, toFloat h)
  in collage w h (
      (rect w' h' |> filled (rgb 174 238 238)) ::
      (rect w' 50 |> filled (rgb 74 163 41)
                  |> move (0, 24 - h'/2)) ::
      (List.map (marioImage h') marios))

-- MARIO
-- fps : number -> Signal Time
-- arrows : Signal { x : Int, y : Int }
input : Signal (Time, ArrowKeys, (Int, Int), Bool)
input = let delta = Signal.map (\t -> t/20) (fps 25)
            _ = { fps' = (fps 25) } |> Debug.watch "fps"
        in  Signal.sampleOn delta (Signal.map4 (,,,) delta Keyboard.arrows Window.dimensions Mouse.isDown)

main : Signal.Signal Element
main = Signal.map2 renders
        Window.dimensions
        (Signal.foldp update [mario0] input)
