import FRP.Reactive
import FRP.Reactive.LegacyAdapters
import Control.Applicative
import Data.Fixed
import Text.Printf
import Data.Monoid

type Progress   = Behavior Percentage
type Percentage = Double
type Width      = Int

renderProgress :: Width -> Progress -> IO ()
renderProgress w p = adaptE $ render <$> renderBar w percent
  where percent :: Event Percentage
        percent = p `snapshot_` atTimes [0.1, 0.2 ..]

render :: String -> IO ()
render ss = putChar '\r' >> putStr ss

renderBar :: Width -> Event Percentage -> Event String
renderBar w = fmap bar
  where bar :: Percentage -> String
        bar p = replicate (truncate (fromIntegral w*p/100)) '#'

renderMeter :: Event Percentage -> Event String
renderMeter = fmap meter
  where meter :: Percentage -> String
        meter p = printf "%3.0f" p ++ " %"

linerProgress :: Progress
linerProgress = liftA2 mod' (time*10) (pure 100)

main :: IO ()
main = renderProgress 30 linerProgress