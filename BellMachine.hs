import FRP.Reactive
import FRP.Reactive.LegacyAdapters
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Monoid
import System.IO

type Bell = Event () -> Event ()

bell :: BellMachine
bell = id

beepTimer :: BellMachine
beepTimer = pure $ atTimes [0, 2..]

nifty :: BellMachine
nifty = beepTimer `mappend` bell

bellAction :: a -> Action
bellAction = const $ putStrLn "BEEP!"

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  (sink, event) <- makeEvent =<< makeClock
  forkIO $ forever $ getChar >> sink ()
  adaptE $ bell <$> nifty event
