import FRP.Reactive
import FRP.Reactive.LegacyAdapters
import Control.Applicative

type Velocity = Double
type Position = Double

data Car = Car { vel :: Velocity, pos :: Position } deriving Show

velocity :: Behavior Velocity
velocity = 1

position :: Behavior Position
position = integral (atTimes [0, 0.5 ..]) velocity

car :: Behavior Car
car = Car <$> velocity <*> position

main :: IO ()
main = adaptE $ print <$> car `snapshot_` atTimes [0, 0.5..]
