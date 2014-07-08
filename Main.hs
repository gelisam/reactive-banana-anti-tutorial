module Main where

import Graphics.Gloss


main :: IO ()
main = play (InWindow "Nice Window" (200, 200) (800, 200))
            white
            30
            ()
            (\() -> Circle 80)
            (\_ () -> ())
            (\_ () -> ())
