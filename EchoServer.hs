import FRP.Reactive
import FRP.Reactive.LegacyAdapters
import System.IO
import Control.Monad
import Control.Concurrent
import Control.Applicative
import Network
import Data.Monoid
import System.Exit
import Control.Exception
import Prelude hiding (catch)

-- pure reactors
echo :: Event String -> Event String
echo = fmap id

timedEcho :: Event String -> Event (String, TimeT)
timedEcho = withTimeE . echo

counterEcho :: Event String -> Event (String, Int)
counterEcho = countE . echo

-- Action-valued reactors
hPrintE :: Show a => Handle -> Event a -> Event Action
hPrintE = fmap . hPrint

printE :: Show a => Event a -> Event Action
printE = hPrintE stdout

quitE :: Handle -> Event (String, Int) -> Event Action
quitE h = fmap (quit h)
  where quit :: Handle -> (String, Int) -> Action
        quit h ("quit\r", _) = hClose h >> exitSuccess
        quit h (_, 10)       = hClose h >> exitFailure
        quit h _             = return ()

-- runner
runEcho :: Show a => Handle -> (Event String -> Event a) -> IO ()
runEcho h echo = do
  (sink, event) <- makeEvent =<< makeClock
  forkIO $ forever $ hGetLine h >>= sink
  adaptE $ (hPrintE `mappend` quitE) h `mappend` printE $ counterEcho event

main :: IO ()
main = withSocketsDo $ listenOn (PortNumber 10001) >>= handler

handler :: Socket -> IO ()
handler s = forever $ do
  (h, _, _) <- accept s
  hSetBuffering h NoBuffering
  forkIO $ runEcho h counterEcho
