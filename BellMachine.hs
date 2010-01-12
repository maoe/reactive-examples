import FRP.Reactive                ( Event, atTime, atTimes )
import FRP.Reactive.LegacyAdapters ( Action, adaptE )
import Control.Applicative         ( (<$>) )

beepTimer :: Event ()
beepTimer = atTime 3

beepTimer' :: Event ()
beepTimer' = atTimes [1..]

bellAction :: a -> Action
bellAction = const $ putStrLn "BEEP!"

main :: IO ()
main = adaptE $ bellAction <$> beepTimer'
