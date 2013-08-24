
module Sound.Comb (



    ) where


import Data.Int
import Foreign.Ptr
import Foreign.C.Types
import Control.Monad
import Foreign.Storable
import Sound.PortAudio
import Control.Concurrent (threadDelay)


main = do                  
    -- putStrLn "Starting up"
    -- putStrLn "Using default stream..."
    initialize
    stream <- 
        openDefaultStream 1 2 44100 (Just 64) 
            (Just $ callback) 
            (Just $ putStrLn "Finished")

    case stream of 
        Left e  -> do
            putStrLn ("Error: " ++ show e)
            terminate
            return ()
        Right s -> do
            putStrLn "Starting stream"
            startStream s
            threadDelay 150000000
            -- stopStream s
            -- threadDelay 1000000
            -- terminate
            return ()
    return ()
    
callback info flags count inp outp = do
        -- fix types
        return (inp::Ptr CFloat, outp::Ptr CFloat)

        forM_ [0..(fromIntegral count - 1)] $ \n -> do 
            v <- peekElemOff inp n
            pokeElemOff outp (n*2)   (v*0.3)
            pokeElemOff outp (n*2+1) (v*0.3)
            return ()
        return Continue
