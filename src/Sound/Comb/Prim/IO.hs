
{-# LANGUAGE NoMonomorphismRestriction, BangPatterns, MultiParamTypeClasses #-}

-- |
-- This implementation is similar to "Sound.Comb.Prim.Ref", but runs all state
-- in 'IO' and uses "Data.Vector.Unboxed" for buffers.
--
module Sound.Comb.Prim.IO (
        State,
        newState,
        step,
        run,
        runVec,
        writeSignal
) where

import Control.Applicative
import Control.Monad (forM_)
import Data.IORef
import Data.Vector.Unboxed (Vector, MVector)
import Data.Vector.Unboxed.Mutable (IOVector)
import System.Random
import Foreign hiding (defPart)
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as MVector

import Sound.File.Sndfile
import Sound.PortAudio
import Sound.PortAudio.Base(PaStreamCallbackTimeInfo)

import Sound.Comb.Prim.Common
import Sound.Comb.Util.Part
import Sound.Comb.Util.Misc



--  A signal is a function of inputs and time over some local state
--  Note that input/outputs may include global buffers
--  TODO higher-order, signals of signals (switching)

data State  = State {
        -- Current input values (index [0,1..])
        stateInputs     :: IOVector Double,

        -- Current and previous bus values (index [-1,-2..])
        stateBuses      :: IOVector Double,
        
        stateCount      :: IORef Int,
        stateRate       :: IORef Double
    }

newState :: IO State
newState = do
    is <- MVector.replicate kMaxInputs 0
    bs <- MVector.replicate (kMaxBuses*kMaxDelay) 0
    c <- newIORef 0
    r <- newIORef 44100
    return $ State is bs c r

-- | @readSamp channel state@
readSamp :: Int -> State -> IO Double
readSamp c s = if c > 0 then readActualInput c s else readBus (neg c) s

-- | @writeSamp delay channel value state@
writeSamp :: Int -> Int -> Double -> State -> IO ()
writeSamp n c = writeBus n (neg c)

-- | Advance state count
incState :: State -> IO ()
incState s = modifyIORef (stateCount s) succ

-- | Current time.
stateTime :: State -> IO Double
stateTime s = do
    sc <- readIORef $ stateCount s
    sr <- readIORef $ stateRate s
    return $ fromIntegral sc / sr

-- | Random value
stateRandom :: State -> IO Double
stateRandom _ = randomRIO (-1,1)

-- --------------------------------------------------------------------------------
-- Internal state stuff

readActualInput :: Int -> State -> IO Double
readActualInput c s = MVector.unsafeRead (stateInputs s) c

readBus :: Int -> State -> IO Double
readBus c s = do
    bp <- bufferPointer s
    MVector.unsafeRead (stateBuses s) (indexBus (bp, c))

{-
Write with some delay.
Buses are always read at bufferPointer

Writing with delay 0 is an error
Writing with delay n writes at @(bufferPointer+n) `mod` kMaxDelay@
-}
writeBus :: Int -> Int -> Double -> State -> IO ()
writeBus n c x s = do
    bp <- bufferPointer s
    MVector.unsafeWrite (stateBuses s) (indexBus (bp + n `mod` kMaxDelay, c)) x

bufferPointer :: State -> IO Int
bufferPointer s = do
    sc <- readIORef $ stateCount s
    return $ sc `mod` kMaxDelay

indexBus :: (Int,Int) -> Int
indexBus (n,c) = c*kMaxDelay + n

kMaxInputs = 1024
kMaxBuses  = 20
kMaxDelay  = 44100*60*5

--------------------------------------------------------------------------------

-- |
-- Verify that a signal can be run. Only works on simplified signals.
--
verify :: Signal -> State -> Bool
verify a s = True
    && requiredDelay a <= kMaxDelay
    && requiredBuses a <= kMaxBuses
    && requiredInputs a <= kMaxInputs

-- |
-- Run a signal over a state. Only works on simplified signals.
--
-- Note that the signal is the first argument, which is usually applied once The resulting
-- function is then unfolded to yield the outputs. We might think of the repeated s
-- application as 'run time'
--
step :: Signal -> State -> IO Double
step = go
    where
        go Random !s           = {-# SCC "random" #-}   stateRandom s
        go Time !s             = {-# SCC "time" #-}     stateTime s
        go (Constant x) !s     = {-# SCC "constant" #-} return x
        go (Lift _ f a) !s     = {-# SCC "lift" #-}     do
            !xa <- a `step` s
            return $ f xa
        go (Lift2 _ f a b) !s  = {-# SCC "lift2" #-}    do
            !xa <- a `step` s
            !xb <- b `step` s
            return $ f xa xb

        go (Input c) !s      = {-# SCC "input" #-}      readSamp c s
        go (Output n c a) !s = {-# SCC "output" #-}     do
            xa <- a `step` s
            writeSamp n c xa s
            return $ xa

        go _ _ = error "step: Unknown signal type, perhaps you forgot simplify"





-- put :: Signal -> IO ()
-- put a = mapM_ (putStrLn.toBars) $ take 60 $ run a

run :: Int -> Signal -> IO [Double]
run n a = Vector.toList <$> runVec n a

runVec :: Int -> Signal -> IO (Vector Double)
runVec n a = do
    s      <- newState
    let a2 = optimize (simplify a)
    let v  = (simplify a) `verify` s
    if not v then error "runVec: Could not verify signal" else 
        Vector.generateM n $ \i -> do
            incState s
            a2 `step` s


--------------------------------------------------------------------------------

-- Sndfile I/O

instance Buffer Vector Double where
    fromForeignPtr = error "fromForeignPtr"

    toForeignPtr !xs = do
        let len = Vector.length xs
        p <- mallocBytes (sizeOf (undefined::Double) * len)
        forM_ [0 .. len - 1] $ \n -> do
            pokeElemOff p n ((Vector.!) xs n)
            return ()
        fp <- newForeignPtr_ p
        return (fp, 0, len)



instance Buffer [] Double where
    fromForeignPtr = error "fromForeignPtr"

    toForeignPtr !xs = do
        let len = length xs
        p <- mallocBytes (sizeOf (undefined::Double) * len)
        forM_ [0 .. len - 1] $ \n ->
            pokeElemOff p n (xs !! n)
        fp <- newForeignPtr_ p
        return (fp, 0, len)

writeSignal :: FilePath -> Signal -> IO ()
writeSignal path a = do
    buffer <- runVec (44100*10) $! a
    Sound.File.Sndfile.writeFile info path buffer
    return ()
        where
            info   = Info {
                    frames      = (44100*10),
                    samplerate  = 44100,
                    channels    = 1,
                    format      = Format
                                    HeaderFormatWav
                                    SampleFormatDouble
                                    EndianCpu,
                    sections    = 1,
                    seekable    = True
                }

