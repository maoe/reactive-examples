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

draw :: Behavior Double -> Behavior Scale -> Behavior String
draw d s = replicate <$> (truncate <$> scaled) <*> pure '#'
  where scaled = s * (d + 1)

sinWave :: Event () -> Behavior String
sinWave ev       = draw sinB 30 `switcher` (constant <$ once ev)
  where constant = draw 0    30 `switcher` (loop <$ once (restE ev))
        loop     = sinWave (restE (restE ev))

sinWave' :: Event () -> Behavior String
sinWave' = cycleB [ draw sinB 30
                  , draw 0    30
                  , draw 1    30 ]

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
  adaptE $ putStrLn <$> sinWave' event `snapshot_` (atTimes [0, 0.2 ..])
