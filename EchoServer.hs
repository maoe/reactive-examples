import FRP.Reactive
import FRP.Reactive.LegacyAdapters
import System.IO
import Control.Applicative
import Control.Monad
import Control.Concurrent
import Network
import Data.Monoid

-- pure reactors
echo :: Event String -> Event String
echo = id

strip :: Event String -> Event String
strip = fmap $ filter (/= '\r')

-- Action-valued reactors
hPutStrLnE :: Handle -> Event String -> Event Action
hPutStrLnE = fmap . hPutStrLn

putStrLnE :: Event String -> Event Action
putStrLnE = hPutStrLnE stdout

quitE :: ThreadId -> Handle -> Event String -> Event Action
quitE t h = fmap $ quit h
  where quit :: Handle -> String -> Action
        quit h "quit" = term t h
        quit _ _      = return ()
        term t h      = killThread t >> hClose h >> myThreadId >>= killThread

-- runner
runEcho :: Handle -> IO ()
runEcho h = do
  (sink, event) <- makeEvent =<< makeClock
  t <- forkIO $ forever $ hGetLine h >>= sink
  adaptE $ mconcat [ hPutStrLnE h, quitE t h, putStrLnE ] $ echo (strip event)

main :: IO ()
main = withSocketsDo $ listenOn (PortNumber 10001) >>= handler

handler :: Socket -> IO ()
handler s = forever $ do
  (h, _, _) <- accept s
  hSetBuffering h NoBuffering
  forkIO $ runEcho h
