import FRP.Reactive
import FRP.Reactive.LegacyAdapters
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Monoid
import Network
import System.IO

main :: IO ()
main = runEchoServer echoServer

runEchoServer :: (Handle -> EchoServer) -> IO ()
runEchoServer e = socketServer >>= adaptE . fmap (forkIO_ . handleConnection e)

socketServer :: IO (Event Handle)
socketServer = withSocketsDo $ do
  (sink, event) <- makeEvent =<< makeClock
  sock <- listenOn (PortNumber 10000)
  forkIO $ forever $ accept sock >>= \(h, _, _) -> hSetBuffering h NoBuffering >> sink h
  return event

handleConnection :: (Handle -> EchoServer) -> Handle -> Action
handleConnection srv h = do
  (sink, event) <- makeEvent =<< makeClock
  forkIO $ forever $ hGetLine h >>= sink
  adaptE $ srv h event

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


-- utilities
forkIO_ :: IO () -> IO ()
forkIO_ act = () <$ forkIO act
