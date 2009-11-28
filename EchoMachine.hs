import FRP.Reactive
import FRP.Reactive.LegacyAdapters
import Control.Concurrent
import Data.List
import Data.Monoid

type EchoMachine = Event String -> Event String

echo :: EchoMachine
echo = id

succEcho :: EchoMachine
succEcho = fmap (map succ)

reverseEcho :: EchoMachine
reverseEcho = fmap reverse

botEcho :: EchoMachine
botEcho = const $ listE $ zip [0, 2 ..] $ repeat "bot"

echoT :: Event String -> Event (String, TimeT)
echoT = withTimeE . echo

succEchoT :: Event String -> Event (String, TimeT)
succEchoT = withTimeE . succEcho

reverseEchoT :: Event String -> Event (String, TimeT)
reverseEchoT = withTimeE . reverseEcho

botEchoT :: Event String -> Event (String, TimeT)
botEchoT = withTimeE . botEcho

runMachine :: Show a => (Event String -> Event a) -> IO ()
runMachine machine = do
  (sink, event) <- makeEvent =<< makeClock
  forkIO $ getInput sink
  adaptE $ fmap print $ machine event

getInput :: Sink String -> IO ()
getInput sink = sequence_ $ repeat (getLine >>= sink)

main :: IO ()
main = do
  -- runMachine $ mconcat [ echo, succEcho, reverseEcho, botEcho ]
  runMachine $ mconcat [ echoT, succEchoT, reverseEchoT, botEchoT ]