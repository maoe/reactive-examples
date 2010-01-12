import FRP.Reactive                ( Event, atTime, atTimes )
import FRP.Reactive.LegacyAdapters ( Action, adaptE )
import Control.Applicative         ( (<$>) )

bellTimer :: Event ()
bellTimer = atTime 3

bellTimer' :: Event ()
bellTimer' = atTimes [1..]

bellAction :: () -> Action
bellAction = const $ putStrLn "BEEP!"

main :: IO ()
main = adaptE $ bellAction <$> bellTimer'
