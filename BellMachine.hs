import FRP.Reactive
import FRP.Reactive.LegacyAdapters
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Monoid
import System.IO

type BellMachine = Event () -> Event ()

doorBell :: BellMachine
doorBell = id

eggTimer :: BellMachine
eggTimer = pure $ atTimes [1..]

nifty :: BellMachine
nifty = eggTimer `mappend` doorBell

runMachine :: BellMachine -> IO ()
runMachine machine = do
  (sink, event) <- makeEvent =<< makeClock
  forkIO $ buttonPresses sink
  adaptE $ fmap bell $ machine event

buttonPresses :: Sink () -> IO ()
buttonPresses sink = sequence_ $ repeat (getChar >> sink ())

bell :: a -> Action
bell = const $ print "BEEP!"

main :: IO ()
main = runMachine nifty
