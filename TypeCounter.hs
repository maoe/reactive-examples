import FRP.Reactive
import FRP.Reactive.LegacyAdapters
import Control.Concurrent
import Control.Applicative
import Data.List
import System.IO
import Data.Char (isDigit)
import Data.Maybe
import Data.Monoid

typeCounter :: Event Char -> Event Integer
typeCounter ev = countB ev `snapshot_` ev

{-
counterE :: Event Char -> Event Integer
counterE ev = counter (numToIntE ev) `snapshot_` ev

counter :: Event Integer -> Behavior Integer
counter ev = 0 `stepper` ev

numToInt :: Char -> Integer
numToInt c | isDigit c = read $ pure c
           | otherwise = 0

numToIntE :: Event Char -> Event Integer
numToIntE = fmap numToInt
-}

runCounter :: IO ()
runCounter = do
  (sink, event) <- makeEvent =<< makeClock
  forkIO $ keyPresses sink
  adaptE $ print <$> typeCounter event

keyPresses :: Sink Char -> Action
keyPresses sink = forever $ getChar >>= sink

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  runCounter
