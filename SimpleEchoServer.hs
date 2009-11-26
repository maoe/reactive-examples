import Control.Applicative
import Control.Concurrent
import Network
import System.IO
import Control.Monad

-- Normal Echo Server
main :: IO ()
main = withSocketsDo $
  listenOn (PortNumber 10000) >>= handler

handler :: Socket -> IO ()
handler s = forever $ do
  (h, _, _) <- accept s
  hSetBuffering h NoBuffering
  forkIO $ echo h

echo :: Handle -> IO ()
echo h = do
  text <- filter (/= '\r') <$> hGetLine h
  if text == "exit"
     then hClose h
     else hPutStrLn h text >> echo h
