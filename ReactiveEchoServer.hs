import FRP.Reactive
import FRP.Reactive.LegacyAdapters
import System.IO
import Control.Monad
import Control.Concurrent
import Network
import Data.Monoid

-- pure reactors
echo :: Event String -> Event String
echo = id

timedEcho :: Event String -> Event (String, TimeT)
timedEcho = withTimeE . echo

counterEcho :: Event String -> Event (String, Int)
counterEcho = countE . echo

-- Action-valued reactors
hPrintE :: Show a => Handle -> Event a -> Event Action
hPrintE = fmap . hPrint

printE :: Show a => Event a -> Event Action
printE = hPrintE stdout

quitE :: ThreadId -> Handle -> Event (String, Int) -> Event Action
quitE t h = fmap (quit h)
  where quit :: Handle -> (String, Int) -> Action
        quit h ("quit\r", _) = terminate t h
        quit h (_, 10)       = terminate t h
        quit _ _             = return ()
        terminate t h = killThread t >> hClose h >> myThreadId >>= killThread

-- runner
runEcho :: Handle -> IO ()
runEcho h = do
  (sink, event) <- makeEvent =<< makeClock
  t <- forkIO $ forever $ hGetLine h >>= sink
  adaptE $ (hPrintE `mappend` quitE t) h `mappend` printE $ counterEcho event

main :: IO ()
main = withSocketsDo $ listenOn (PortNumber 10001) >>= handler

handler :: Socket -> IO ()
handler s = forever $ do
  (h, _, _) <- accept s
  hSetBuffering h NoBuffering
  forkIO $ runEcho h
