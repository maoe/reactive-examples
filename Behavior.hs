import FRP.Reactive
import FRP.Reactive.LegacyAdapters
import System.IO
import Control.Concurrent
import Control.Applicative
import Data.Monoid

-- pure Behaviors
typeCounter :: Event Char -> Behavior Int
typeCounter = countB

resettableTypeCounter :: Event Char -> Behavior Int
resettableTypeCounter ev = typeCounter ev `switcher` fmap (resetEvent (typeCounter ev)) ev
  where resetEvent :: Num a => Behavior a -> Char -> Behavior a
        resetEvent _   'r' = pure 0
        resetEvent beh  _  = beh

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