import FRP.Reactive                (Event, atTimes)
import FRP.Reactive.LegacyAdapters (Action, adaptE)
import Control.Applicative         ((<$>))

beepTimer :: Event ()
beepTimer = atTimes [0 ..]

bellAction :: a -> Action
bellAction = const $ putStrLn "BEEP!"

main :: IO ()
main = adaptE $ bellAction <$> beepTimer
