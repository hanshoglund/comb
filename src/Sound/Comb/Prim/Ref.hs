
{-# LANGUAGE NoMonomorphismRestriction, BangPatterns, MultiParamTypeClasses #-}

-- |
-- Reference implementation, using a pure state monad and "Data.Map" for buffers.
--
-- Should not be used except for testing and validating new functionality.
--
module Sound.Comb.Prim.Ref (
        State,
        defState,
        step,
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

-- | Current time.
stateTime :: State -> Double
stateTime s = fromIntegral (stateCount s) / stateRate s

-- | Random value
stateRandom :: State -> (Double, State)
stateRandom s = (x, s {stateRandomGen = g}) where (x, g) = randomR (-1,1::Double) (stateRandomGen s)

-- | @readSamp channel state@
readSamp :: Int -> State -> Double
readSamp c s = if c > 0 then readActualInput c s else readBus (neg c) s

-- | @writeSamp delay channel value state@
writeSamp :: Int -> Int -> Double -> State -> State
writeSamp n c = writeBus n (neg c)

-- | Advance state count
incState :: State -> State
incState x = x { stateCount = stateCount x + 1 }

--------------------------------------------------------------------------------
-- Internal state stuff

readActualInput :: Int -> State -> Double
readActualInput c s = fromMaybe 0 $
    Map.lookup c (stateInputs s)

readBus :: Int -> State -> Double
readBus c s = fromMaybe 0 $
    Map.lookup (bufferPointer s, c) (stateBuses s)

{-
Write with some delay.
Buses are always read at bufferPointer

Writing with delay 0 is an error
Writing with delay n writes at (bufferPointer+n)
-}
writeBus :: Int -> Int -> Double -> State -> State
writeBus n c x s
    | n <= 0    = error "writeBus: Negative or zero delay."
    | otherwise = s { stateBuses = Map.insert (bufferPointer s + n, c) x (stateBuses s) }

bufferPointer :: State -> Int
bufferPointer s = stateCount s `mod` kMaxDelay

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
step :: Signal -> State -> (Double, State)
step = go
    where
        go Random !s           = {-# SCC "random" #-}   stateRandom s
        go Time !s             = {-# SCC "time" #-}     (stateTime s, s)
        go (Constant x) !s     = {-# SCC "constant" #-} (x, s)

        go (Lift _ f a) !s     = {-# SCC "lift" #-}     let
            (!xa, !sa) = a `step` s
            in (f xa, sa)
        go (Lift2 _ f a b) !s  = {-# SCC "lift2" #-}    let
            (!xa, !sa) = a `step` s
            (!xb, !sb) = b `step` sa
            in (f xa xb, sb)
            -- TODO could be more parallel with non-sequential state

        go (Input c) !s      = {-# SCC "input" #-}      (readSamp c s, s)
        go (Output n c a) !s = {-# SCC "output" #-}     let
            (xa, sa) = a `step` s
            in (xa, writeSamp n c xa sa)

        go _ _ = error "step: Unknown signal type, perhaps you forgot simplify"



run :: Signal -> [Double]
run a = let s = defState
    in if not (a `verify` s) then error "Could not verify" else 
        unfoldr (runBase a) s

runVec :: Int -> Signal -> Vector Double
runVec n a = Vector.unfoldrN n (runBase a) defState

runBase :: Signal -> State -> Maybe (Double, State)
runBase a = Just . fmap incState . step a2
    where       
        !a2 = optimize (simplify a)


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

