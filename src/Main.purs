module Main where

import Prelude
import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array
import Data.Maybe (Maybe(..))
import Graphics.Canvas
import Math as Math

type Radius   = Number
type Color    = String
type Position = { x   :: Number,   y      :: Number }
type Circle   = { pos :: Position, radius :: Radius }

width  = 600.0
height = 600.0

radius = 250.0

centerX = width  / 2.0
centerY = height / 2.0

type Angle = Number

nums :: Array Number
nums = [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0,21.0,22.0,23.0,24.0]

posToAng :: Number -> Angle
posToAng n = (/) (Math.pi * 2.0) n

angToPos :: Angle -> Position
angToPos ang = {x: ((Math.cos ang) * radius) + 300.0, y: ((Math.sin ang) * radius) + 300.0}

space = Math.pi * 2.0 / 24.0

angles = map ((*) space) nums

drawCircle :: forall e. Circle -> Color -> Context2D -> Eff (canvas :: CANVAS | e) Context2D
drawCircle cir col ctx = do
  let a = { x: cir.pos.x, y: cir.pos.y, r: cir.radius, start: 0.0, end: Math.pi * 2.0 }
  _ <- beginPath ctx
  _ <- arc ctx a
  _ <- setFillStyle col ctx
  fill ctx

main :: forall e. (Partial) => Eff (canvas :: CANVAS, console :: CONSOLE | e) Unit
main = do
  Just canvas <- getCanvasElementById "circle"
  ctx         <- getContext2D canvas
  _           <- drawCircle { pos: { x: 300.0, y: 300.0 }, radius: 250.0 } "#F44336" ctx
  foreachE angles \n -> void do
    let pos = angToPos n
    let cir = {pos: pos, radius: 10.0}
    drawCircle cir "black" ctx
