import FRP.Reactive
import FRP.Reactive.LegacyAdapters
import Control.Applicative
import System.IO
import Control.Concurrent
import Control.Monad
import Data.Monoid

position :: Event Char -> Behavior Double
position ev = integral rate (velocity ev)
  where rate = everySecE

velocity :: Event Char -> Behavior Double
velocity ev = 0 `accumB` (controller <$> ev)

controller :: Char -> (Double -> Double)
controller '>' = (+) 0.1
controller '<' = flip (-) (0.1)
controller '0' = const 0
controller  _  = id

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin  NoBuffering
  hSetEcho stdin False
  (sink, event) <- makeEvent =<< makeClock
  forkIO $ forever $ getChar >>= sink
  adaptE $ print <$> position event `snapshot_` everySecE

everySecE = atTimes [0..]