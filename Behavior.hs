import FRP.Reactive
import FRP.Reactive.LegacyAdapters
import Control.Applicative
import Control.Concurrent
import Data.Char
import Data.Monoid
import System.IO

-- pure Behaviors
typeCounter :: Event Char -> Behavior Int
typeCounter = countB

resettableTypeCounter :: Event Char -> Behavior Int
resettableTypeCounter ev = accumB 0 (fmap control ev)
  where control :: Char -> Int -> Int
        control c | isDigit c = const (read [c])
        control 'r'           = const 0
        control 'i'           = id
        control  _            = succ

-- Action-valued Events
printE :: Show a => Event a -> Event Action
printE = fmap print

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  (sink, event) <- makeEvent =<< makeClock
  forkIO $ sequence_ $ repeat $ getChar >>= sink
  -- adaptE $ printE $ typeCounter event `snapshot_` event
  adaptE $ printE $ resettableTypeCounter event `snapshot_` event
