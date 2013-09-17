
{-# LANGUAGE NoMonomorphismRestriction, BangPatterns, MultiParamTypeClasses #-}

module Sound.Comb.Prim.IO (
        State,
        newState,
        step,
        -- put,
        -- run,
        runVec,
        runBase,
        writeSignal
) where

import Data.Int
import Data.Monoid
import Data.Maybe
import Data.IORef
import Data.Foldable (foldMap)
import Foreign hiding (defPart)
import Control.Monad (forM_)
import Data.List (mapAccumL, transpose, unfoldr)
import Data.Tree      
import System.Random hiding (random)
import Sound.File.Sndfile

-- import Sound.PortAudio
import Sound.PortAudio.Base(PaStreamCallbackTimeInfo)
import Control.Concurrent (threadDelay)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Vector.Unboxed (Vector, MVector)
import Data.Vector.Unboxed.Mutable (IOVector)
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as MVector






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
    -- deriving (Show)

newState :: IO State
newState = do
    is <- MVector.replicate kMaxBuses 0
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
-- -- Internal state stuff

readActualInput :: Int -> State -> IO Double 
readActualInput c s = MVector.unsafeRead (stateInputs s) c 

readBus :: Int -> State -> IO Double 
readBus c s = do
    bp <- bufferPointer s
    MVector.unsafeRead (stateBuses s) (indexBus (bp, c))

-- readBus c s = fromMaybe 0 $ 
--     Map.lookup (bufferPointer s, c) (stateBuses s)

-- 
-- -- Write with some delay.
-- -- Buses are always read at bufferPointer
-- --
-- -- Writing with delay 0 is an error
-- -- Writing with delay n writes at (bufferPointer+n)
-- 
writeBus :: Int -> Int -> Double -> State -> IO ()
writeBus n c x s = do
    bp <- bufferPointer s
    MVector.unsafeWrite (stateBuses s) (indexBus (bp + n, c)) x

-- writeBus n c x s 
--     | n <= 0    = error "writeBus: Negative or zero delay."
--     | otherwise = s { stateBuses = Map.insert (bufferPointer s + n, c) x (stateBuses s) }

bufferPointer :: State -> IO Int
bufferPointer s = do
    sc <- readIORef $ stateCount s
    return $ sc `mod` kMaxDelay

indexBus :: (Int,Int) -> Int
indexBus (n,c) = c*kMaxDelay + n

kMaxBuses = 20
kMaxDelay = 44100*60*5

--------------------------------------------------------------------------------


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

-- run :: Signal -> [Double]                               
-- run a = unfoldr (runBase a) defState

runVec :: Int -> Signal -> IO (Vector Double)
runVec n a = do
    s      <- newState
    let a2 = (optimize . simplify) a
    Vector.generateM n $ \i -> do
        incState s
        step a2 s

runBase :: Signal -> State -> Maybe (Double, State)
runBase = undefined
-- runBase a = Just . fmap incState . swap . step a2
    -- where                                                
        -- !a2        = (optimize . simplify) a


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
                           
