module Main where

import Data.Monoid
import Graphics.Gloss
import Graphics.Gloss.Data.Extent


extentR, extentA, extentB, extentC :: Extent
extentR = makeExtent   90    60  65 (-65)
extentA = makeExtent   40    10  65 (-65)
extentB = makeExtent (-10) (-40) 65 (-65)
extentC = makeExtent (-60) (-90) 65 (-65)

main :: IO ()
main = play (InWindow "Nice Window" (200, 200) (800, 200))
            white
            30
            (1, 2, 3)
            render
            (const id)
            (const id)

render :: (Int, Int, Int) -> Picture
render (xA, xB, xC) = button extentR "Refresh"
                   <> button extentA (show xA)
                   <> button extentB (show xB)
                   <> button extentC (show xC)

button :: Extent -> String -> Picture
button ex s = color azure bg <> color white fg
  where
    bg = polygon (cornerPoints ex)
    fg = translate x y
       $ uscale 0.1
       $ translate (-150) (-50)  -- vertically centered, random x offset :(
       $ text s
    (x, y) = c2p (centerCoordOfExtent ex)


uscale :: Float -> Picture -> Picture
uscale v = scale v v


c2p :: Coord -> Point
c2p (x,y) = (fromIntegral x, fromIntegral y)

cornerCoords :: Extent -> [Coord]
cornerCoords ex = [(w,n), (e,n), (e,s), (w,s)]
  where
    (n, s, e, w) = takeExtent ex

cornerPoints :: Extent -> [Point]
cornerPoints = map c2p . cornerCoords
