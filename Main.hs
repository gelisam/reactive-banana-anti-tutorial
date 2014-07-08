module Main where

import Graphics.Gloss


main :: IO ()
main = display (InWindow "Nice Window" (200, 200) (800, 200)) white (Circle 80)
