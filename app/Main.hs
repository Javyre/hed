module Main where

import           Lib
import           Graphics.Vty
import qualified Graphics.Vty (Event)

data Event = TermEvent Vty.Event -- | OtherEvent todo

newtype EventQueue = EventQueue [Event]

doStuff vty (EventQueue eq) = loop 5
    where loop 0 = return ()
          loop n = do 
              putStrLn "recur!"
              loop $ n - 1

consumeTermEvts :: Vty -> IO EventQueue
consumeTermEvts vty = loop []
    where loop q = do
        evt <- nextEventNonBlocking

mainLoop :: IO ()
mainLoop vty = loop $ EventQueue []
    where
        loop evts = do
            newEvts <- consumeTermEvts vty

            let nextq = evts ++ newEvts
             in loop nextq
        

main :: IO ()
main = do
    cfg <- standardIOConfig
    vty <- mkVty cfg
    doStuff vty
    shutdown vty
    putStrLn "test"
