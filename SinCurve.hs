import FRP.Reactive
import FRP.Reactive.LegacyAdapters
import Control.Applicative
import System.IO

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

main :: IO ()
main = do
  adaptE $ putStrLn <$> sinCurve 30 `snapshot_` (atTimes [0, 0.2 ..])
  -- adaptE $ putStrLn <$> cosCurve 30 `snapshot_` (atTimes [0, 0.2 ..])
  -- adaptE $ putStrLn <$> poweredSinCurve 3 30 `snapshot_` (atTimes [0, 0.2 ..])
  -- adaptE $ putStrLn <$> poweredSinCurve 3 30 `snapshot_` (atTimes [0, 0.2 ..])
