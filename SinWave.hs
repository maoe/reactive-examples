import FRP.Reactive
import FRP.Reactive.LegacyAdapters
import Control.Applicative

sinB :: Behavior Double
sinB = sin <$> time

cosB :: Behavior Double
cosB = cos <$> time

draw :: Behavior Double -> Scale -> Behavior String
draw d s = replicate <$> (truncate <$> scaled) <*> pure '#'
  where scaled = pure s * (d + 1)

type Scale = Double

sinWave :: Scale -> Behavior String
sinWave = draw sinB

cosWave :: Scale -> Behavior String
cosWave = draw sinB

poweredSinWave :: Integral a => a -> Scale -> Behavior String
poweredSinWave n = draw $ sinB^n

poweredCosWave :: Integral a => a -> Scale -> Behavior String
poweredCosWave n = draw $ cosB^n

main :: IO ()
main = do
  adaptE $ putStrLn <$> sinWave 30 `snapshot_` atTimes [0, 0.2 ..]
  -- adaptE $ putStrLn <$> cosWave 30 `snapshot_` atTimes [0, 0.2 ..]
  -- adaptE $ putStrLn <$> poweredSinWave 3 30 `snapshot_` atTimes [0, 0.2 ..]
  -- adaptE $ putStrLn <$> poweredSinWave 3 30 `snapshot_` atTimes [0, 0.2 ..]
