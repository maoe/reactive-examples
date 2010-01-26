import FRP.Reactive
import FRP.Reactive.LegacyAdapters
import Data.Char
import Control.Applicative
import System.IO
import Control.Concurrent
import Control.Monad
import Data.Monoid

type Velocity = Double
type Position = Double

data Car = Car { vel :: Velocity, pos :: Position } deriving Show

velocity :: Event Char -> Behavior Velocity
velocity ev = 0 `accumB` fmap controller ev

position :: Behavior Velocity -> Behavior Position
position = integral everySecE

car :: Event Char -> Behavior Car
car ev = Car <$> v <*> p
  where v = velocity ev
        p = position v

controller :: Char -> Double -> Double
controller '>' = succ             -- accelerate
controller '<' = pred             -- slow down
controller ' ' = (* (-1))         -- turn back
controller c
  | isDigit c  = const $ read [c] -- set the speed
controller  _  = id               -- keep the speed

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin  NoBuffering
  hSetEcho stdin False
  (sink, event) <- makeEvent =<< makeClock
  forkIO $ forever $ getChar >>= sink
  adaptE $ print <$> car event `snapshot_` everySecE

everySecE = atTimes [0..]
