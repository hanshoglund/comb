
{-# LANGUAGE RankNTypes, TypeOperators, DeriveFunctor, GADTs, MultiParamTypeClasses #-}

module Sound.Comb -- (
--    ) 
where

import Data.IORef
import Data.Int
import Data.Fixed
import System.Random
import Data.Functor.Contravariant
import Foreign.Ptr
import Foreign.C.Types
import Control.Applicative
import Control.Monad
import Foreign.Storable

import Sound.PortAudio
import Sound.PortAudio.Base(PaStreamCallbackTimeInfo)

import Control.Concurrent (threadDelay)

-- TODO use non-default audio device






import qualified Sound.File.Sndfile as Sndfile


-- Bogus instances
instance Sndfile.Buffer [] Double where
    fromForeignPtr p i j = return []
    toForeignPtr _ = return (error "No function: toForeignPtr")
                    

main = do                  
    putStrLn "Reading sound file test.wav"
    (info, bogusBuffer) <- Sndfile.readFile "test.wav"
    let _ = (bogusBuffer :: Maybe [Double])
    putStrLn $ "  Sample rate: " ++ (show $ Sndfile.samplerate info)
    putStrLn $ "  Number of channels: " ++ (show $ Sndfile.channels info)

    putStrLn "Starting up PortAudio"
    putStrLn "  Using default stream..."
    initialize
    sampleCount <- newIORef 0
    stream <- 
        openDefaultStream 1 2 44100 (Just 64) -- inputs outputs sr vecSize 
            (Just $ callback sampleCount) 
            (Just $ putStrLn "  Finished")

    case stream of 
        Left e  -> do
            putStrLn ("  Error: " ++ show e)
            terminate
            return ()
        Right s -> do
            putStrLn "  Starting stream"
            startStream s
            threadDelay 150000000
            -- stopStream s
            -- threadDelay 1000000
            -- terminate
            return ()
            putStrLn "  Stream has finished"
    return ()

callback        :: IORef CULong
                -- rest is PA parameters
                -> PaStreamCallbackTimeInfo
                -> [StreamCallbackFlag] 
                -> CULong 
                -> Ptr CFloat 
                -> Ptr CFloat 
                -> IO StreamResult

callback sampleCount info_ flags_ count inp outp = do
        -- count is <= vecSize above, usually equal

        forM_ [0..(fromIntegral count - 1)] $ \n -> do
            -- v <- peekElemOff inp n

            totalSamples <- fmap (+ fromIntegral n) (readIORef sampleCount)

            let v = (cos . (*tau)) $ ((realToFrac totalSamples/44100*440) `mod'` 1)
            -- v <- fmap toFull randomIO

            -- putStrLn (toBars (v::CFloat))

            pokeElemOff outp (n*2)   (v*0.3) -- left output
            pokeElemOff outp (n*2+1) (v*0.3) -- right output
            return ()

        modifyIORef sampleCount (+ count)
        return Continue

--  A signal is a function of inputs and time over some local state
--  Note that input/outputs may include global buffers
--  TODO strictness, turn networks on and off (stepping)
--  TODO higher-order, signals of signals (switching)
type Time   = Int
type Signal = forall s . Time -> s -> [Float] -> (s, [Float])



toFull x = (x*2)-1
toPos x  = (x+1)/2

-- view as bars if in range (-1,1)
toBars :: RealFrac a => a -> String
toBars x = case round (toPos x * 20) of
    0  -> ".                    |"
    1  -> " .                   |"
    2  -> "  .                  |"
    3  -> "   .                 |"
    4  -> "    .                |"
    5  -> "     .               |"
    6  -> "      .              |"
    7  -> "       .             |"
    8  -> "        .            |"
    9  -> "         .           |"
    10 -> "          .          |"
    11 -> "           .         |"
    12 -> "            .        |"
    13 -> "             .       |"
    14 -> "              .      |"
    15 -> "               .     |"
    16 -> "                .    |"
    17 -> "                 .   |"
    18 -> "                  .  |"
    19 -> "                   . |"
    20 -> "                    .|"

tau = 2 * pi