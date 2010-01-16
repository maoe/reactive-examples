import FRP.Reactive
import FRP.Reactive.LegacyAdapters
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Monoid
import Network
import System.IO

main :: IO ()
main = withSocketsDo $ listenOn (PortNumber 10001) >>= handler

handler :: Socket -> IO ()
handler s = forever $ do
  (h, _, _) <- accept s
  hSetBuffering h NoBuffering
  forkIO $ runEcho h

runEcho :: Handle -> IO ()
runEcho h = do
  (sink, event) <- makeEvent =<< makeClock
  forkIO $ forever $ hGetLine h >>= sink
  adaptE $ echoServer h event

type EchoServer = Event String -> Event Action

-- a combined reactor
echoServer :: Handle -> EchoServer
echoServer h = mconcat [ hPutStrLnE h . fmap reverse
                       , putStrLnE
                       , quitE h
                       ] . stripE

-- pure reactors
stripE :: Event String -> Event String
stripE = fmap $ filter (/= '\r')

reverseE :: Event String -> Event String
reverseE = fmap reverse

-- Action-valued reactors
quitE :: Handle -> EchoServer
quitE = fmap . quit
  where quit :: Handle -> String -> Action
        quit h "quit" = hClose h
        quit _ _      = return ()

hPutStrLnE :: Handle -> EchoServer
hPutStrLnE = fmap . hPutStrLn

putStrLnE :: EchoServer
putStrLnE  = hPutStrLnE stdout

