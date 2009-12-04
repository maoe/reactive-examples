import FRP.Reactive
import FRP.Reactive.LegacyAdapters
import Control.Concurrent
import Control.Applicative
import Data.List
import System.IO
import System.Exit (exitSuccess)
import Text.Printf (printf)
import Data.VectorSpace
import FRP.Reactive.Num

type Meter = Behavior Accel
type Accel    = Double
type Velocity = Double
type Position = Double

type KeyReactor = Event Char -> Event Command

data Command = Quit | Status | SpeedUp | SpeedDown | Help | Other Char
               deriving Show

accel :: Event Accel -> Behavior Accel
accel = stepper 0

keyMapping :: Char -> Command
keyMapping 'q' = Quit
keyMapping 's' = Status
keyMapping ' ' = Status
keyMapping '>' = SpeedUp
keyMapping '<' = SpeedDown
keyMapping 'h' = Help
keyMapping  c  = Other c

keyReactor :: KeyReactor
keyReactor = fmap keyMapping

runMeter :: KeyReactor -> IO ()
runMeter reactor = do
  (sink, event) <- makeEvent =<< makeClock
  forkIO $ keyPresses sink
  adaptE $ invoker <$> reactor event

invoker :: Command -> Action
invoker Quit      = exitSuccess
invoker (Other c) = putStrLn $ printf "unknown command %c" c
invoker c         = putStrLn $ printf "execute command %s" (show c)

keyPresses :: Sink Char -> Action
keyPresses sink = sequence_ $ repeat $ getChar >>= sink

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  runMeter keyReactor