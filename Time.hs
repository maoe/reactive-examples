import FRP.Reactive
import FRP.Reactive.LegacyAdapters
import Control.Applicative

main :: IO ()
main = adaptE $ print <$> time `snapshot_` atTimes [0..]
