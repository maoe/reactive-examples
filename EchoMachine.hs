import FRP.Reactive                ( Event, countE, listE, withTimeE )
import FRP.Reactive.LegacyAdapters ( makeClock, makeEvent, adaptE )
import Control.Applicative         ( (<$>) )
import Control.Concurrent          ( forkIO )
import Control.Monad               ( forever )
import Data.Monoid                 ( Monoid(mconcat) )

type EchoMachine a = Event String -> Event a

echo :: EchoMachine String
echo = id

succEcho :: EchoMachine String
succEcho = fmap (map succ)

reverseEcho :: EchoMachine String
reverseEcho = fmap reverse

botEcho :: EchoMachine String
botEcho = const $ listE $ zip [0, 2 ..] $ repeat "bot"

runMachine :: Show a => EchoMachine a -> IO ()
runMachine machine = do
  (sink, event) <- makeEvent =<< makeClock
  forkIO $ forever $ getLine >>= sink
  adaptE $ print <$> machine event

main :: IO ()
main = do
  let machine = mconcat [ echo, succEcho, reverseEcho, botEcho ]
  -- runMachine machine
  -- runMachine $ withTimeE <$> machine
  runMachine $ countE . withTimeE <$> machine
