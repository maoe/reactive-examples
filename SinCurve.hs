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

powerSinCurve :: Integral a => a -> Behavior Scale -> Behavior String
powerSinCurve n = drawCurve ((sin <$> time)^n)

powerCosCurve :: Integral a => a -> Behavior Scale -> Behavior String
powerCosCurve n = drawCurve ((cos <$> time)^n)

main :: IO ()
main = adaptE $ putStrLn <$> powerSinCurve 3 50 `snapshot_` (atTimes [0.1, 0.2 ..])
