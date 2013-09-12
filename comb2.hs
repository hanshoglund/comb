
{-# LANGUAGE NoMonomorphismRestriction, BangPatterns, MultiParamTypeClasses #-}

module Main
where

import Data.Int
import Data.Monoid
import Data.Maybe
import Foreign hiding (new)
import Control.Monad (forM_)
import Data.List (mapAccumL, transpose, unfoldr)
import Data.Tree      
import Sound.File.Sndfile

-- import Sound.PortAudio
import Sound.PortAudio.Base(PaStreamCallbackTimeInfo)
import Control.Concurrent (threadDelay)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Vector.Unboxed (Vector, MVector)
import qualified Data.Vector.Unboxed as Vector



-- Generate unique sets
-- Laws:

-- > gen new = [1..]
-- > genAll a `merge` genAll b = genAll c   iff   (a,b) = split c
-- > genAll A is distinct from genAll b

type Gen a = (a,a) -- offset, diff

new   :: Num a => Gen a
gen   :: Num a => Gen a -> (Gen a, a)
split :: Num a => Gen a -> (Gen a, Gen a)


new           = (0,1)
gen     (o,d) = ((o+d,d), o)
split   (o,d) = ((o,d*2), (d,d*2))


next :: Num a => Gen a -> a
skip :: Num a => Gen a -> Gen a
next = snd . gen
skip = fst . gen

genAll :: Num a => Gen a -> [a]
genAll g = let
    (g2,x) = gen g
    in x : genAll g2





--  A signal is a function of inputs and time over some local state
--  Note that input/outputs may include global buffers
--  TODO higher-order, signals of signals (switching)
type Time   = Int
data State  = State {
        -- current input values (index [0,1..])
        stateInputs     :: Map Int Double,

        -- current bus values (index [-1,-2..])
        stateBuses      :: Map Int Double,
        stateCount      :: Int,             -- processed samples
        stateRate       :: Double           -- samples per second
    }
    deriving (Show)

-- instance Default State where
    -- def = State mempty mempty 0 44100 

defState :: State
defState = State mempty mempty 0 44100

-- channel state    
readInput :: Int -> State -> Double 
readInput n s = fromMaybe 0 $ Map.lookup n2 m
    where
        (n2, m) = if n > 0 then (n, stateInputs s) else (negate $ n+1, stateBuses s)

-- delay channel value state
writeOutput :: Int -> Int -> Double -> State -> State 
writeOutput n c x s = s { stateBuses = Map.insert n2 x m } 
    where
        (n2, m) = (negate $ c+1, stateBuses s)


data Signal
    = Time
    | Constant Double
    | Lift  String (Double -> Double) Signal                  -- string is optional name
    | Lift2 String (Double -> Double -> Double) Signal Signal -- string is optional name

    | Loop (Signal -> Signal)
    | Delay Signal

    -- >= 0 means real (global) input
    -- <  0 means local (feedback) input

    | Input Int 

    -- only used for feedback for now
    | Output Int Signal

instance Eq Signal where
    (==) = error "No (==)"
instance Ord Signal where
    compare = error "No compare"
    max = lift2' "max" max
    min = lift2' "min" min
instance Num Signal where
    (+) = lift2' "(+)" (+)
    (*) = lift2' "(*)" (*)
    (-) = lift2' "(-)" (-)
    abs           = lift' "abs" abs
    signum        = lift' "signum" signum
    fromInteger x = signal (fromInteger x)
instance Fractional Signal where
    recip = lift' "recip" recip
    fromRational x = signal (fromRational x)
instance Show Signal where
    show = drawTree . signalTree
instance Floating Signal where
    pi      = signal pi
    exp     = lift' "exp" exp
    sqrt    = lift' "sqrt" sqrt
    log     = lift' "log" log
    (**)    = lift2' "(**)" (**)
    logBase = lift2' "logBase" logBase
    sin     = lift' "sin" sin
    tan     = lift' "tan" tan
    cos     = lift' "cos" cos
    asin    = lift' "asin" asin
    atan    = lift' "atan" atan
    acos    = lift' "acos" acos
    sinh    = lift' "sinh" sinh
    tanh    = lift' "tanh" tanh
    cosh    = lift' "cosh" cosh
    asinh   = lift' "asinh" asinh
    atanh   = lift' "atanh" atanh
    acosh   = lift' "acosh" acosh


signalTree :: Signal -> Tree String
signalTree = go . simplify
    where
        go Time             = Node "time" []
        go (Constant x)     = Node (show x) []
        go (Lift n _ a)     = Node n [signalTree a]
        go (Lift2 n _ a b)  = Node n [signalTree a, signalTree b]
        go (Input n)        = Node ("input " ++ show n) []
        go (Output n a)     = Node ("output " ++ show n) [signalTree a] 

time    :: Signal
input   :: Int -> Signal
signal  :: Double -> Signal
lift    :: (Double -> Double) -> Signal -> Signal
lift2   :: (Double -> Double -> Double) -> Signal -> Signal -> Signal
former  :: Signal -> Signal -> Signal -- run both in given order, return first arg
latter  :: Signal -> Signal -> Signal -- run both in given order, return second arg
loop    :: (Signal -> Signal) -> Signal
delay :: Int -> Signal -> Signal
time    = Time
input   = Input
signal  = Constant
lift    = Lift "f"
lift2   = Lift2 "f"
lift'   = Lift
lift2'  = Lift2
latter  = Lift2 "latter" (\_ x -> x)
former  = Lift2 "former" (\x _ -> x)
loop    = Loop
delay 0 = id
delay n = delay1 . delay (n - 1)
    where
        delay1 = Delay

impulse = lift' "mkImp" (\x -> if x == 0 then 1 else 0) time


biquad :: Signal -> Signal -> Signal -> Signal -> Signal -> Signal -> Signal
biquad b0 b1 b2 a1 a2 x = loop $ \y -> b0*x + b1*delay 1 x + b2*delay 2 x 
    - a1*delay 1 y - a2*delay 2 y

-- |
-- Recursively remove signal constructors not handled by 'step'.
-- 
-- Currently, it replaces:
--
--   * All loops with local input/outputs
--   * All delays with local input/output pair
--
simplify :: Signal -> Signal
simplify = go new
    where
        go g (Loop f)        = out $ go h (f inp)
            where                     
                out   = Output i
                inp   = Input i
                i     = neg $ next g
                h     = skip g
        go g (Delay a)        = inp `former` out
            where
                out = Output i (go h a)
                inp = Input i
                i   = neg $ next g
                h   = skip g
                
        go g (Lift n f a)     = Lift n f (go g a)
        go g (Lift2 n f a b)  = Lift2 n f (go g1 a) (go g2 b) where (g1, g2) = split g
        -- Note: split is unnecessary if evaluation is sequential

        go g x = x                                     

        neg x = negate (x + 1)

put :: Signal -> IO ()
put a = mapM_ (putStrLn.toBars) $ take 60 $ run a

run :: Signal -> [Double]                               
run a = unfoldr (runBase a) defState

runVec :: Int -> Signal -> Vector Double                               
runVec n a = Vector.unfoldrN n (runBase a) defState

-- runBase :: Signal -> State -> Maybe (Double, State)
-- runBase a x = (Just . fmap incState . swap . step a) x
--     where
--         incState x = x { stateCount = stateCount x + 1 }

runBase :: Signal -> State -> Maybe (Double, State)
runBase a = Just . fmap incState . swap . step a2
    where                                                
        !a2        = (optimize . simplify) a
        incState x = x { stateCount = stateCount x + 1 }

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
        go Time !s             = {-# SCC "time" #-}     (s, fromIntegral (stateCount s) / stateRate s) 
        go (Constant x) !s     = {-# SCC "constant" #-} (s, x)
 
        go (Lift _ f a) !s     = {-# SCC "lift" #-}     let 
            (!sa, !xa) = a `step` s 
            in (sa, f xa)
        go (Lift2 _ f a b) !s  = {-# SCC "lift2" #-}    let
            (!sa, !xa) = a `step` s
            (!sb, !xb) = b `step` sa 
            in (sb, f xa xb)      
            -- TODO could be more parallel with non-sequential state
 
        go (Input c) !s      = {-# SCC "input" #-}      (s, readInput c s)
        go (Output c a) !s   = {-# SCC "output" #-}     let 
            (sa, xa) = a `step` s
            in (writeOutput 1 c xa sa, xa)
        go _ _ = error "step: Unknown signal type, perhaps you forgot simplify"


optimize :: Signal -> Signal
optimize = go
    where
        go (Lift2 "(*)" _ (Constant 0) b) = 0
        go (Lift2 "(*)" _ a (Constant 0)) = 0
        go (Lift2 "(*)" _ (Constant 1) b) = b
        go (Lift2 "(*)" _ a (Constant 1)) = a

        go (Lift2 "(+)" _ (Constant 0) b) = b
        go (Lift2 "(+)" _ a (Constant 0)) = a
        go (Lift2 "(-)" _ (Constant 0) b) = b
        go (Lift2 "(-)" _ a (Constant 0)) = a

        go (Lift2 "(/)" _ (Constant 0) a) = 0
        go (Lift2 "(/)" _ a (Constant 0)) = error "optimize: Division by zero"

        go a = a



-- | From range (0,1) to range (-1,1)
toFull :: Num a => a -> a
toFull x = (x*2)-1

-- | From range (-1,1) to range (0,1)
toPos  :: Fractional a => a -> a
toPos x  = (x+1)/2


-- Could be more general if not due to MonoMorph..R
-- toBars :: RealFrac a => a -> String

-- | View as bars if in range (-1,1)
toBars :: Double -> String
toBars x = let n = round (toPos x * width) in
    if n > width || n < 0
        then replicate (width+1) ' ' ++ "|"
        else replicate n ' ' ++ "." ++ replicate (width-n) ' ' ++ "|"
    where 
        width = 80


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
    let buffer = runVec numSampls $! a
    Sound.File.Sndfile.writeFile info path buffer
    return ()
        where              
            info   = Info {
                    frames      = numSampls,
                    samplerate  = sr,
                    channels    = 1,
                    format      = Format 
                                    HeaderFormatWav 
                                    SampleFormatDouble 
                                    EndianCpu,
                    sections    = 1,
                    seekable    = True
                }

main :: IO ()
main = do
    writeSignal "test.wav" sig 
    putStrLn "Finished"


major freq = (sin (freq*4) + sin (freq*5) + sin (freq*6))*0.02
        
sig = delay 10 (sum $ fmap (\x -> major $ freq*x) [1,3/2,4/5,6/7,8/9,10/11,11/12,13/14,15/16,17/18])

freq = time*440            
numSampls = sr * secs
secs = 10
sr   = 44100 -- TODO see stateRate above




tau                 = 2 * pi
first  f (a,b)      = (f a, b)
second f (a,b)      = (a, f b)
swap (a,b)          = (b, a)
dup x               = (x, x)



