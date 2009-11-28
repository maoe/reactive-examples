import FRP.Reactive
import FRP.Reactive.LegacyAdapters
import Control.Applicative
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

runMachine :: Show a => (Event String -> Event a) -> IO ()
runMachine machine = do
  (sink, event) <- makeEvent =<< makeClock
  forkIO $ getInput sink
  adaptE $ fmap print $ machine event

getInput :: Sink String -> IO ()
getInput sink = sequence_ $ repeat (getLine >>= sink)

main :: IO ()
main = do
  let machine = mconcat [ echo, succEcho, reverseEcho, botEcho ]
  -- runMachine machine
  -- runMachine $ withTimeE <$> machine
  runMachine $ countE . withTimeE <$> machine
  -- runMachine $ snapshot time . echo
