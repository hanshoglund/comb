
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


-- forall s . (Semigroup s, Monoid s, Typeable s) => s -> Time -> [Float] -> (s, Float)


--  A signal is a function of inputs and time over some local state
--  Note that input/outputs may include global buffers
--  TODO strictness, turn networks on and off (stepping)
--  TODO higher-order, signals of signals (switching)
type Time   = Int
data State  = State Float State State
instance Monoid State where
    mempty = State 0 mempty mempty
    mappend = error "No mappend"

readBuf  :: State -> Float
readBuf (State x s t) = x

writeBuf :: Float -> State -> State
writeBuf x (State _ s t) = State x s t

splitState :: State -> (Float, State, State)
splitState (State x s t) = (x, s, t)

mergeState :: Float -> State -> State -> State
mergeState x s t = State x s t

upState :: State -> State
upState s = State 0 s mempty

downState :: State -> State
downState (State x s t) = s



newtype Signal = Signal { getSignal ::
    State -> Time -> [Float] -> (State, Float)
    }
instance Num Signal where
    a + b = a `addS` b
    a * b = a `mulS` b
    negate        = mulS $ always (-1)
    abs           = liftS abs
    signum        = liftS signum
    fromInteger x = always (fromInteger x)
instance Fractional Signal where
    recip = liftS recip
    fromRational x = always (fromRational x)

-- Constant value
always :: Float -> Signal
always x = Signal $ \s t _ -> (s, x)

-- Time in samples
time :: Signal
time = Signal $ \s t _ -> (s, fromIntegral t)

-- Input number n
input  :: Int -> Signal
input n = Signal $ \s t ins -> (s, ins !! n)

-- Lift pure unary func
liftS :: (Float -> Float) -> Signal -> Signal
liftS f (Signal a) = Signal $ \s t i -> fmap f (a s t i)

sinS = liftS sin
addS = lift2S (+)
mulS = lift2S (*)

-- Lift pure binary func
lift2S :: (Float -> Float -> Float) -> Signal -> Signal -> Signal
lift2S op (Signal a) (Signal b) = Signal $ \s t i -> let
    (u,p,q)  = splitState s
    (sa,xa) = a p t i
    (sb,xb) = b q t i 
    in (mergeState u sa sb, xa `op` xb)
    -- Is <> right for combining states
    -- Alternatively, we might run the state transformations in sequence
    -- Anyhow, state transformations should not interact, how to assure this?

delay2   = delay . delay    
delay    = loop $ \o a -> (a, o)
recur f  = loop (\x -> dup . f x)

-- Recursive transform, similar to scanl
-- Function have form (\fb new -> (fb, res))
loop :: (Float -> Float -> (Float, Float)) -> Signal -> Signal
loop op (Signal a) = Signal $ \s t i -> let 
    (sa,xa) = first downState $ a (upState s) t i
    xo      = readBuf sa
    (fb,xc) = xo `op` xa
    in (writeBuf fb sa, xc)



-- |
-- Run a signal starting at time 0 and default state, ignoring output state
--
-- > runS signal inputs => output
--
runS :: Signal -> [[Float]] -> [Float]
runS (Signal a) inputs = snd $ mapAccumL proc (mempty::State) (zip [0..] inputs)
    where
        proc s (t, xs) = a s t xs

test :: IO ()
test = mapM_ (putStrLn.toBars) $ runS sig inp
    where    
        -- one channel input
        inp = (fmap.fmap) (/ 10) $ concat $ replicate 4 $ transpose [[-10..10]]
                       
        sig = lift2S (\a b -> a) (delay $ input 0) (input 0)
        -- sig = (delay.delay.delay.delay.delay) (input 0)

first  f (a,b)      = (f a, b)
second f (a,b)      = (a, f b)
cast'               = fromJust . cast
fromJust (Just x)   = x
dup x               = (x, x)





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


tau = 2 * pi


