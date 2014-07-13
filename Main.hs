{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Applicative
import Control.Arrow
import Data.List hiding (union)
import Data.Monoid
import Data.Tuple
import Graphics.Gloss
import Graphics.Gloss.Data.Extent
import Graphics.Gloss.Interface.FRP.ReactiveBanana
import Graphics.Gloss.Interface.Pure.Game hiding (Event)
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Reactive.Banana.Switch


extentR, extentA, extentB, extentC :: Extent
extentR = makeExtent   90    60  65 (-65)
extentA = makeExtent   40    10  65 (-65)
extentB = makeExtent (-10) (-40) 65 (-65)
extentC = makeExtent (-60) (-90) 65 (-65)

main :: IO ()
main = playBanana (InWindow "Nice Window" (200, 200) (800, 200))
                  white
                  30
                  reactiveMain

reactiveMain :: forall t. Frameworks t
             => Event t Float
             -> Event t InputEvent
             -> Moment t (Behavior t Picture)
reactiveMain floats events = return pictures
  where
    beginning :: Event t ()
    beginning = voidE (firstEvent floats)
    
    refreshClicks :: Event t ()
    refreshClicks = buttonClick extentR events
    
    buttonClicks :: [Event t ()]
    buttonClicks = map (flip buttonClick events) buttons
    
    addFakeClicks :: Event t () -> Event t ()
    addFakeClicks realClicks = unions [beginning, refreshClicks, realClicks]
    
    fakeButtonClicks :: [Event t ()]
    fakeButtonClicks = map addFakeClicks buttonClicks
    
    labelledClicks :: [Event t Char]
    labelledClicks = zipWith (fmap . const) ['a'..] fakeButtonClicks
    
    clickLabels :: Event t Char
    clickLabels = unions labelledClicks
    
    clickEvents :: Event t (Char, Int)
    clickEvents = fmap swap
                $ numberEvents
                $ clickLabels
    
    textA,textB,textC :: Behavior t String
    [textA,textB,textC] = map textN "abc"
    
    textN :: Char -> Behavior t String
    textN label = stepper "loading..." (loadingN `union` delayedCountN)
      where
        countN, loadingN, delayedCountN :: Event t String
        countN = fmap (show . snd)
               $ filterE ((== label) . fst)
               $ clickEvents
        loadingN = fmap (const "loading...") countN
        delayedCountN = delayE floats 0.5 countN
    
    buttonTexts :: Behavior t (String,String,String)
    buttonTexts = liftA3 (,,) textA textB textC
    
    pictures :: Behavior t Picture
    pictures = fmap render buttonTexts


voidE :: Event t a -> Event t ()
voidE = fmap (const ())


data Schedule a = Schedule
  { currentEvents :: [a]
  , futureEvents :: [(Float, a)]
  }
  deriving Show

instance Monoid (Schedule a)
  where
    mempty = Schedule [] []
    Schedule xs txs `mappend` Schedule ys tys
      = Schedule (xs `mappend` ys) (txs `mappend` tys)

scheduleEvent :: Float -> a -> Schedule a
scheduleEvent t x = Schedule [] [(t,x)]

advanceSchedule :: forall a. Float -> Schedule a -> Schedule a
advanceSchedule dt s = Schedule (map snd triggered) remaining
  where
    decreasedEvents, triggered, remaining :: [(Float, a)]
    decreasedEvents = map (first (subtract dt)) (futureEvents s)
    (triggered, remaining) = partition ((<= 0) . fst) decreasedEvents

delayE :: forall t a. Event t Float -> Float -> Event t a -> Event t a
delayE dts delay events = delayedEvents
  where
    scheduledEvents :: Event t (Schedule a)
    scheduledEvents = scheduleEvent delay <$> events
    
    eventSchedulers :: Event t (Schedule a -> Schedule a)
    eventSchedulers = mappend <$> scheduledEvents
    
    scheduleAdvancers :: Event t (Schedule a -> Schedule a)
    scheduleAdvancers = advanceSchedule <$> dts
    
    scheduleModifiers :: Event t (Schedule a -> Schedule a)
    scheduleModifiers = eventSchedulers `union` scheduleAdvancers
    
    delayedSchedule :: Event t (Schedule a)
    delayedSchedule = accumE mempty scheduleModifiers
    
    delayedEvents :: Event t a
    delayedEvents = spill (currentEvents <$> delayedSchedule)


firstEvent :: Event t a -> Event t a
firstEvent = fmap snd
           . filterE ((== 1) . fst)
           . numberEvents

-- (1, x1), (2, x2), ...
numberEvents :: Event t a -> Event t (Int, a)
numberEvents = accumZip 0 (+1)


accumZip :: a -> (a -> a) -> Event t b -> Event t (a, b)
accumZip zero suc = accumE (zero, undefined)
                  . fmap go
  where
    go y (x,_) = (suc x, y)

accumZipWith :: (a -> b -> c) -> a -> (a -> a) -> Event t b -> Event t c
accumZipWith f zero suc = fmap (uncurry f) . accumZip zero suc


buttons :: [Extent]
buttons = [extentA, extentB, extentC]

buttonClick :: Extent -> Event t InputEvent -> Event t ()
buttonClick ex = voidE . filterE isInside
  where
    isInside (EventKey (MouseButton LeftButton) Down _ p) = pointInExtent ex p
    isInside _                                            = False

renderFloat :: Float -> Picture
renderFloat = uscale 0.2 . text . show

render :: (String, String, String) -> Picture
render (xA, xB, xC) = button extentR "Refresh"
                   <> button extentA xA
                   <> button extentB xB
                   <> button extentC xC

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
