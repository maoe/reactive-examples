import FRP.Reactive
import FRP.Reactive.LegacyAdapters
import Control.Applicative
import System.IO
import Control.Concurrent
import Control.Monad
import Data.Monoid
import Data.List

sinB :: Behavior Double
sinB = sin <$> time

type Scale = Double

draw :: Behavior Double -> Scale -> Behavior String
draw d s = replicate <$> (truncate <$> scaled) <*> pure '#'
  where scaled = pure s * (d + 1)

sinWave :: Event () -> Behavior Double
sinWave ev    = sinB `switcher` (next  <$ once ev)
  where next  = 0    `switcher` (next' <$ once (restE ev))
        next' = 1    `switcher` (loop  <$ once (restE (restE ev)))
        loop  = sinWave $ restE $ restE $ restE ev

sinWave' :: Event () -> Behavior Double
sinWave' = cycleB [ sinB, 0, 1 ]

cycleB :: [Behavior a] -> Event e -> Behavior a
cycleB bs ev = loop ev bs
  where loop e []      = cycleB bs e
        loop e (b:bs') = b `switcher` (loop (restE e) bs' <$ once e)

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  (sink, event) <- makeEvent =<< makeClock
  forkIO $ forever $ getChar >> sink () >> putStrLn "A key is pressed"
  adaptE $ putStrLn <$> draw (sinWave' event) 30 `snapshot_` atTimes [0, 0.2 ..]
