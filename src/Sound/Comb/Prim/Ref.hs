
{-# LANGUAGE NoMonomorphismRestriction, BangPatterns, MultiParamTypeClasses #-}

-- |
-- Reference implementation, using a pure state monad.
--
module Sound.Comb.Prim.Ref (
        State,
        defState,
        step,
        put,
        run,
        runVec,
        runBase,
        writeSignal
) where

import Data.Monoid
import Data.Maybe
import Data.IORef
import Foreign hiding (defPart)
import Control.Monad (forM_)
import Data.List (mapAccumL, transpose, unfoldr)
import System.Random hiding (random)

import Sound.PortAudio
import Sound.File.Sndfile
import Sound.PortAudio.Base(PaStreamCallbackTimeInfo)
import Control.Concurrent (threadDelay)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Vector.Unboxed (Vector, MVector)
import Data.Vector.Unboxed.Mutable (IOVector)
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as MVector


import Sound.Comb.Util.Part
import Sound.Comb.Util.Misc
import Sound.Comb.Prim.Common


--------------------------------------------------------------------------------
-- Implementation

--  A signal is a function of inputs and time over some local state
--  Note that input/outputs may include global buffers
--  TODO higher-order, signals of signals (switching)

data State  = State {
        -- Current input values (index [0,1..])
        stateInputs     :: Map Int Double,
        
        -- Current and previous bus values (index [-1,-2..])
        stateBuses      :: Map (Int,Int) Double,
        stateCount      :: Int,             -- processed samples
        stateRate       :: Double,          -- samples per second
        
        stateRandomGen  :: StdGen           -- random number source
    }
    deriving (Show)

defState :: State
defState = State mempty mempty 0 44100 (mkStdGen 198712261455)

-- prefilledBuses :: Map (Int,Int) Double
-- prefilledBuses = Map.fromList $! [ (a,b) | a <- [1..1000], b <- [-1,-2..negate kMaxBuses]] `zip` (repeat 0)

-- | @readSamp channel state@
readSamp :: Int -> State -> Double
readSamp c s = if c > 0 then readActualInput c s else readBus (neg c) s

-- | @writeSamp delay channel value state@
writeSamp :: Int -> Int -> Double -> State -> State 
writeSamp n c = writeBus n (neg c)

-- | Advance state count
incState :: State -> State
incState x = x { stateCount = stateCount x + 1 }

-- | Current time.
stateTime :: State -> Double
stateTime s = fromIntegral (stateCount s) / stateRate s

-- | Random value
stateRandom :: State -> (State, Double)
stateRandom s = (s {stateRandomGen = g}, x) where (x, g) = randomR (-1,1::Double) (stateRandomGen s)

--------------------------------------------------------------------------------
-- Internal state stuff

readActualInput :: Int -> State -> Double 
readActualInput c s = fromMaybe 0 $ 
    Map.lookup c (stateInputs s) 

readBus :: Int -> State -> Double 
readBus c s = fromMaybe 0 $ 
    Map.lookup (bufferPointer s, c) (stateBuses s)

-- Write with some delay.
-- Buses are always read at bufferPointer
--
-- Writing with delay 0 is an error
-- Writing with delay n writes at (bufferPointer+n)

writeBus :: Int -> Int -> Double -> State -> State 
writeBus n c x s 
    | n <= 0    = error "writeBus: Negative or zero delay."
    | otherwise = s { stateBuses = Map.insert (bufferPointer s + n, c) x (stateBuses s) }

bufferPointer :: State -> Int
bufferPointer s = stateCount s `mod` kMaxDelay

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
step :: Signal -> State -> (State, Double)
step = go
    where
        go Random !s           = {-# SCC "random" #-}   stateRandom s
        go Time !s             = {-# SCC "time" #-}     (s, stateTime s) 
        go (Constant x) !s     = {-# SCC "constant" #-} (s, x)
 
        go (Lift _ f a) !s     = {-# SCC "lift" #-}     let 
            (!sa, !xa) = a `step` s 
            in (sa, f xa)
        go (Lift2 _ f a b) !s  = {-# SCC "lift2" #-}    let
            (!sa, !xa) = a `step` s
            (!sb, !xb) = b `step` sa 
            in (sb, f xa xb)      
            -- TODO could be more parallel with non-sequential state
 
        go (Input c) !s      = {-# SCC "input" #-}      (s, readSamp c s)
        go (Output n c a) !s = {-# SCC "output" #-}     let 
            (sa, xa) = a `step` s
            in (writeSamp n c xa sa, xa)
        
        go _ _ = error "step: Unknown signal type, perhaps you forgot simplify"





put :: Signal -> IO ()
put a = mapM_ (putStrLn.toBars) $ take 60 $ run a

run :: Signal -> [Double]                               
run a = unfoldr (runBase a) defState

runVec :: Int -> Signal -> Vector Double                               
runVec n a = Vector.unfoldrN n (runBase a) defState

runBase :: Signal -> State -> Maybe (Double, State)
runBase a = Just . fmap incState . swap . step a2
    where                                                
        !a2        = (optimize . simplify) a


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
    let buffer = runVec (44100*10) $! a
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
                           
