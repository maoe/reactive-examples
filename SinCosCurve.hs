import FRP.Reactive
import FRP.Reactive.LegacyAdapters
import Control.Applicative
import System.IO
import Control.Concurrent
import Control.Monad
import Data.Monoid

type Scale = Double

sinB :: Behavior Double
sinB = sin <$> time

cosB :: Behavior Double
cosB = cos <$> time

drawCurve :: Behavior Double -> Behavior Scale -> Behavior String
drawCurve d s = replicate <$> (truncate <$> scaled) <*> pure '#'
  where scaled = s * (d + 1)

sinCurve :: Behavior Scale -> Behavior String
sinCurve = drawCurve (sin <$> time)

cosCurve :: Behavior Scale -> Behavior String
cosCurve = drawCurve (cos <$> time)

poweredSinCurve :: Integral a => a -> Behavior Scale -> Behavior String
poweredSinCurve n = drawCurve ((sin <$> time)^n)

poweredCosCurve :: Integral a => a -> Behavior Scale -> Behavior String
poweredCosCurve n = drawCurve ((cos <$> time)^n)

sinCosCurve :: Event () -> Behavior String
sinCosCurve ev = sinCurve 30
                   `switcher`
                     (cosCurve 30 <$ ev)

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  (sink, event) <- makeEvent =<< makeClock
  forkIO $ forever $ getChar >> sink () >> putStrLn "A key is pressed"
  adaptE $ putStrLn <$> sinCosCurve event
                          `snapshot_` (atTimes [0.2, 0.4 ..])

