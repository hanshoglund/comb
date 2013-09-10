
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
newtype Signal = Signal { getSignal ::
    -- forall s . (Semigroup s, Monoid s, Typeable s) => s -> Time -> [Float] -> (s, Float)
    (s ~ [[Float]]) => s -> Time -> [Float] -> (s, Float)
    }
instance Num Signal where
    a + b = a `addS` b
    a * b = a `mulS` b
    negate        = mulS $ constS (-1)
    abs           = liftS abs
    signum        = liftS signum
    fromInteger x = constS (fromInteger x)
    
instance Fractional Signal where
    recip = liftS recip
    fromRational x = constS (fromRational x)

-- Constant value
constS :: Float -> Signal
constS x = Signal $ \s t i -> (s, x)

-- Time in samples
timeS :: Signal
timeS = Signal $ \s t i -> (s, fromIntegral t)

-- Input number n
inputS  :: Int -> Signal
inputS n = Signal $ \s t i -> (s, i !! n)

-- Lift pure unary func
liftS :: (Float -> Float) -> Signal -> Signal
liftS f (Signal g) = Signal $ \s t i -> second f (g s t i)

sinS = liftS sin
addS = lift2S (+)
mulS = lift2S (*)

-- Lift pure binary func
lift2S :: (Float -> Float -> Float) -> Signal -> Signal -> Signal
lift2S op (Signal f) (Signal g) = Signal $ \s t i -> let 
    (sa,xa) = f s t i
    (sb,xb) = g s t i 
    in (sa <> sb, xa `op` xb)
    -- Is <> right for combining states
    -- Alternatively, we might run the state transformations in sequence
    -- Anyhow, state transformations should not interact, how to assure this?

-- biquad b0 a1 b1 a2 b2 x = recurS $ \y x ->
    -- b0*x + b1*(delayS x) + b2*(delay2S x) - a1*y - a2*(delayS y)

delay2S = delayS . delayS    
delayS = loopS $ \o a -> (a, o)

recurS :: (Float -> Float -> Float) -> Signal -> Signal
recurS f = loopS (\x -> dup . f x)

-- Recursive transform, similar to scanl
-- Function have form (\old new -> res)
loopS :: (Float -> Float -> (Float, Float)) -> Signal -> Signal
loopS op (Signal f) = Signal $ \s t i -> let 
    (sa,xa) = f s t i
    xo      = undefined -- TODO from state
    (sc,xc) = xo `op` xa
    in (sa <> {-sc-}mempty, xc)





{-
-- Note that for non-input (non-reactive) signals, we might juss thift time
-- However to get properly shifted inputs, we need the state    
delaySNR :: Signal -> Signal
delaySNR (Signal f) = Signal $ \s t i -> f s (t-1) i

delayS :: Signal -> Signal
delayS (Signal f) = Signal $ \s t i -> let
    in f (s ++ [i]) (t-1) (if length s < 1 then [0,0..] else (last s))
    -- FIXME
-}
    

-- mapAccumL :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])


-- |
-- Run a signal starting at time 0 and default state, ignoring output state
--
-- > runS signal inputs => output
--
runS :: Signal -> [[Float]] -> [Float]
runS (Signal f) inputs = snd $ mapAccumL proc (mempty::[[Float]]) (zip [0..] inputs)
    where
        proc s (t, xs) = f s t xs

test :: IO ()
test = mapM_ (putStrLn.toBars) $ runS sig inp
    where    
        -- one channel input
        inp = (fmap.fmap) (/ 3) $ concat $ replicate 6 $ [[0],[1],[2],[3],[2],[1],[0],[-1],[-2],[-3],[-2],[-1]]
                       
        sig = sinS $ timeS*0.15
        -- sig = (delayS.delayS.delayS.delayS.delayS) (inputS 0)



second f (a,b) = (a, f b)
cast' = fromJust . cast
fromJust (Just x) = x
dup x = (x, x)
-- TODO sample level


{-

type S = Int -> Double
type P01 =              S
type P11 = S         -> S
type P21 = (S,S)     -> S
type P31 = (S,S,S)   -> S
-}
{-
par   :: (Sa -> Sb) -> (Sc -> Sd) -> S[a+c] -> S[b+d]
seq   :: (Sa -> Sb) -> (Sb -> Sd) -> Sa     -> Sd
split :: P -> P
merge :: P -> P
rec   :: P -> P

(+), (-), (||) :: P21
0, 1, 2        :: P01   
delay :: P11

-}



















toFull :: Num a => a -> a
toFull x = (x*2)-1

toPos  :: Fractional a => a -> a
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