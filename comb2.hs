
{-# LANGUAGE NoMonomorphismRestriction, 
    RankNTypes, TypeOperators, DeriveFunctor, GADTs, MultiParamTypeClasses #-}

module Sound.Comb -- (
--    ) 
where

import Data.IORef
import Data.Int
import Data.Semigroup
import Data.Default
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


--  A signal is a function of inputs and time over some local state
--  Note that input/outputs may include global buffers
--  TODO strictness, turn networks on and off (stepping)
--  TODO higher-order, signals of signals (switching)
type Time   = Int
data State  = State {
        stateInputs     :: [Double],    -- current input values
        stateBuses      :: [Signal],    -- current buses
        stateCount      :: Int,         -- processed samples
        stateRate       :: Double       -- samples per second
    }
instance Default State where
    def = State [] [] 0 10
    
    
-- instance Monoid State where



newtype Signal = Signal { getSignal ::
    State -> (State, Double)
    }
instance Num Signal where
    (+) = lift2 (+)
    (*) = lift2 (*)
    negate        = (* (-1))
    abs           = lift abs
    signum        = lift signum
    fromInteger x = signal (fromInteger x)
instance Fractional Signal where
    recip = lift recip
    fromRational x = signal (fromRational x)

-- Constant value
time    :: Signal
input   :: Int -> Signal
signal  :: Double -> Signal
lift    :: (Double -> Double) -> Signal -> Signal
lift2   :: (Double -> Double -> Double) -> Signal -> Signal -> Signal
loop    :: (Double -> Double -> (Double, Double)) -> Signal -> Signal
delay   :: Signal -> Signal
time        = Signal $ \s -> (s, fromIntegral (stateCount s) / stateRate s)
input n     = Signal $ \s -> (s, stateInputs s !! n)
signal x    = Signal $ \s -> (s, x)
lift f a    = Signal $ \s -> let 
    (sa, xa) = (getSignal a) s 
    in (sa, f xa)
lift2 f a b = Signal $ \s -> let
    (sa, xa) = (getSignal a) s
    (sb, xb) = (getSignal b) sa
    in (sb, f xa xb)
    
loop        = error "Not impl: loop"
delay       = error "Not impl: delay"

runSignal :: Signal -> [[Double]] -> [Double]
runSignal a inputs = snd $ mapAccumL proc def inputs
    where
        proc s as = let 
            s2 = s { 
                stateInputs = as
            }
            (s3, x) = (getSignal a) s2
            s4 = s3 {
                stateCount = stateCount s2 + 1
            }
            in (s4, x)

putSignal :: Signal -> [[Double]] -> IO ()
putSignal a xs = mapM_ (putStrLn.toBars) $ runSignal a xs

main :: IO ()
main = putSignal sig inp
    where    
        -- one channel input
        inp = (fmap.fmap) (/ 10) $ concat $ replicate 3 $ transpose [[-10..10]]
                                        
        sig = time*0.5
        -- sig = lift2 (\a b -> a) (input 0) (input 0)
        -- sig = (delay.delay.delay.delay.delay) (input 0)



-- | From range (0,1) to range (-1,1)
toFull :: Num a => a -> a
toFull x = (x*2)-1

-- | From range (-1,1) to range (0,1)
toPos  :: Fractional a => a -> a
toPos x  = (x+1)/2

-- | View as bars if in range (-1,1)
toBars :: RealFrac a => a -> String
toBars x = let n = round (toPos x * width) in
    if n > width || n < 0
        then replicate (width+1) ' ' ++ "|"
        else replicate n ' ' ++ "." ++ replicate (width-n) ' ' ++ "|"
    where
        width = 80


tau                 = 2 * pi
first  f (a,b)      = (f a, b)
second f (a,b)      = (a, f b)
cast'               = fromJust . cast
fromJust (Just x)   = x
dup x               = (x, x)


