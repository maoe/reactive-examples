import FRP.Reactive
import FRP.Reactive.LegacyAdapters
import System.IO
import Control.Monad
import Control.Concurrent
import Network

echo :: Event String -> Event String
echo = fmap id

runEcho :: Show a => Handle -> (Event String -> Event a) -> IO ()
runEcho h echo = do
  (sink, event) <- makeEvent =<< makeClock
  forkIO $ producer h sink
  adaptE $ consumer h $ echo event

producer :: Handle -> Sink String -> Action
producer h sink = sequence_ $ repeat $ hGetLine h >>= sink

consumer :: Show a => Handle -> Event a -> Event Action
consumer h ev = fmap (hPrint h) ev

main :: IO ()
main = withSocketsDo $ listenOn (PortNumber 10001) >>= handler

handler :: Socket -> IO ()
handler s = forever $ do
  (h, _, _) <- accept s
  hSetBuffering h NoBuffering
  forkIO $ runEcho h echo