import FRP.Reactive
import FRP.Reactive.LegacyAdapters
import Control.Applicative

type Progress = Behavior Percentage
type Percentage = Double
type ProgressBar = Behavior Percentage -> Behavior String

runProgressBar :: Behavior Percentage -> Event String
runProgressBar p = consoleBar p `snapshot_` atTimes [0.1,0.2..]

progressBar :: Int -> ProgressBar
progressBar w = fmap bar
  where bar :: Percentage -> String
        bar p = replicate (truncate $ (p/100)*fromIntegral w) '#'

consoleBar :: ProgressBar
consoleBar = progressBar 80

percent = time * 10

main = adaptE $ putStrLn <$> runProgressBar percent