
{-# LANGUAGE NoMonomorphismRestriction, 
    RankNTypes, TypeOperators, DeriveFunctor, GADTs, MultiParamTypeClasses #-}

module Sound.Comb -- (
--    ) 
where

import Data.IORef
import Data.Int
import Data.Semigroup
import Data.Typeable
import Data.Fixed
import System.Random
import Data.Functor.Contravariant
import Foreign.Ptr
import Foreign.C.Types
import Control.Applicative
import Control.Monad
import Data.List (mapAccumL)
import Foreign.Storable
import Data.List (transpose)

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




