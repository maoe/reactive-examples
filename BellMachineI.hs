import FRP.Reactive                ( Event, atTimes )
import FRP.Reactive.LegacyAdapters ( makeClock, Action, makeEvent, adaptE )
import Control.Applicative         ( Applicative(pure), (<$>) )
import Control.Concurrent          ( forkIO )
import Control.Monad               ( forever )
import Data.Monoid                 ( Monoid(mappend) )
import System.IO                   ( stdout, stdin, hSetBuffering, hSetEcho
                                   , BufferMode(NoBuffering) )

type BellMachine = Event () -> Event ()

bell :: BellMachine
bell = id

bellTimer :: BellMachine
bellTimer = const $ atTimes [0, 2..]

nifty :: BellMachine
nifty = bellTimer `mappend` bell

bellAction :: a -> Action
bellAction = const $ putStrLn "BEEP!"

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  (sink, event) <- makeEvent =<< makeClock
  forkIO $ forever $ getChar >> sink ()
  adaptE $ bellAction <$> nifty event
