import FRP.Reactive
import FRP.Reactive.LegacyAdapters
import Control.Concurrent
import Control.Applicative
import Data.List
import System.IO
import System.Exit (exitSuccess)
import Text.Printf (printf)

type Accel    = Double
type Velocity = Double
type Position = Double

type CommandReactor = Event Char -> Event Command

data Command = Quit
             | Status
             | SpeedUp
             | SpeedDown
             | Accel Accel
             | Velocity Velocity
             | Position Position
             | Help
             | Other Char
               deriving Show

accel :: Event Accel -> Behavior Accel
accel ev = 0 `stepper` ev

velocity :: Event Accel -> Behavior Velocity
velocity ev = integral ev (accel ev)

position :: Event Accel -> Behavior Position
position ev = integral ev (velocity ev)

commandReactor :: CommandReactor
commandReactor = fmap keyMapping

keyMapping :: Char -> Command
keyMapping 'q' = Quit
keyMapping 's' = Status
keyMapping ' ' = Status
keyMapping '>' = SpeedUp
keyMapping '<' = SpeedDown
keyMapping 'h' = Help
keyMapping  c  = Other c

-- runMeter :: KeyReactor -> IO ()
runMeter = do
  (sink, event) <- makeEvent =<< makeClock
  forkIO $ forever $ getChar >>= sink
  adaptE $ invoker <$> commandReactor event

invoker :: Command -> Action
invoker Quit      = exitSuccess
invoker (Other c) = putStrLn $ printf "unknown command %c" c
invoker c         = putStrLn $ printf "execute command %s" (show c)

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  runMeter
