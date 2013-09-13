
{-# LANGUAGE NoMonomorphismRestriction, BangPatterns, MultiParamTypeClasses #-}

module Main
where

import Data.Int
import Data.Monoid
import Data.Maybe
import Data.Foldable (foldMap)
import Foreign hiding (newPart)
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
import qualified Data.Vector.Unboxed as Vector



-- Generate unique sets
-- Laws:

-- > partition newPart = [1..]
-- > partitionAll a `merge` partitionAll b = partitionAll c   iff   (a,b) = splitPart c
-- > partitionAll A is distinct from partitionAll b

type Part a = (a,a) -- offset, diff

newPart   :: Num a => Part a
partition   :: Num a => Part a -> (Part a, a)
splitPart :: Num a => Part a -> (Part a, Part a)


newPart           = (0,1)
partition     (o,d) = ((o+d,d), o)
splitPart   (o,d) = ((o,d*2), (d,d*2))


nextP :: Num a => Part a -> a
skipP :: Num a => Part a -> Part a
nextP = snd . partition
skipP = fst . partition

partitionAll :: Num a => Part a -> [a]
partitionAll g = let
    (g2,x) = partition g
    in x : partitionAll g2





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
        stateRate       :: Double,          -- samples per second
        
        stateRandomGen  :: StdGen           -- random number source
    }
    deriving (Show)

-- instance Default State where
    -- def = State mempty mempty 0 44100 

defState :: State
defState = State mempty mempty 0 44100 (mkStdGen 198712261455)

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
    | Random
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
    (/)   = lift2' "(/)" (/)
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


signalNodeCount :: Signal -> Int
signalNodeCount x = getSum $ foldMap (const (Sum 1)) $ signalTree x

signalTree :: Signal -> Tree String
signalTree = go . simplify
    where
        go Time             = Node "time" []
        go Random           = Node "random" []
        go (Constant x)     = Node (show x) []
        go (Lift n _ a)     = Node n [signalTree a]
        go (Lift2 n _ a b)  = Node n [signalTree a, signalTree b]
        go (Input n)        = Node ("input " ++ show n) []
        go (Output n a)     = Node ("output " ++ show n) [signalTree a] 

time    :: Signal
random  :: Signal
input   :: Int -> Signal
signal  :: Double -> Signal
lift    :: (Double -> Double) -> Signal -> Signal
lift2   :: (Double -> Double -> Double) -> Signal -> Signal -> Signal
former  :: Signal -> Signal -> Signal -- run both in given order, return first arg
latter  :: Signal -> Signal -> Signal -- run both in given order, return second arg
loop    :: (Signal -> Signal) -> Signal
delay :: Int -> Signal -> Signal
time    = Time
random  = Random
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

-- Goes from (0-tau) n times per second.
-- Suitable for feeding a sine oscillator.
line :: Double -> Signal
line n = time*tau*signal n

-- Where did I get this?
-- See also http://www.musicdsp.org/files/Audio-EQ-Cookbook.txt

lowPassC :: Floating a => a -> a -> a -> a -> (a, a, a, a, a)
lowPassC fc fs q peakGain = (a0,a1,a2,b1,b2)
    where
        v = 10 ** abs peakGain / 20
        k = tan (pi * fc / fs)
       
        norm = 1 / (1+k / q+k^2)
        
        a0 = k^2 * norm
        a1 = 2 * a0
        a2 = a0
        b1 = 2 * (k^2 - 1) * norm
        b2 = (1 - k / q + k^2) * norm

lowPass :: Signal -> Signal -> Signal -> Signal -> Signal -> Signal
lowPass fc fs q peakGain = biquad a0 a1 a2 b1 b2
    where                                                           
        (a0,a1,a2,b1,b2) = lowPassC fc fs q peakGain



biquad :: Signal -> Signal -> Signal -> Signal -> Signal -> Signal -> Signal
biquad b0 b1 b2 a1 a2 x = loop $ \y -> 
    b0*x + b1 * delay 1 x + b2 * delay 2 x 
         - a1 * delay 1 y - a2 * delay 2 y

-- |
-- Recursively remove signal constructors not handled by 'step'.
-- 
-- Currently, it replaces:
--
--   * All loops with local input/outputs
--   * All delays with local input/output pair
--
simplify :: Signal -> Signal
simplify = go newPart
    where
        go g (Loop f)        = out $ go h (f inp)
            where                     
                out   = Output i
                inp   = Input i
                i     = neg $ nextP g
                h     = skipP g
        go g (Delay a)        = inp `former` out
            where
                out = Output i (go h a)
                inp = Input i
                i   = neg $ nextP g
                h   = skipP g
                
        go g (Lift n f a)     = Lift n f (go g a)
        go g (Lift2 n f a b)  = Lift2 n f (go g1 a) (go g2 b) where (g1, g2) = splitPart g
        -- Note: splitPart is unnecessary if evaluation is sequential

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
        go Random !s           = {-# SCC "random" #-}   (s {stateRandomGen = g}, x) where (x, g) = randomR (-1,1::Double) (stateRandomGen s)
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


-- |
-- Optimize a signal. Only works on simplified signals.
--
optimize :: Signal -> Signal
optimize = rec . optimize1
    where
        rec (Lift n f a)     = Lift n f (optimize a)
        rec (Lift2 n f a b)  = Lift2 n f (optimize a) (optimize b)
        rec (Output c a)     = Output c (optimize a)
        rec a                = a

optimize1 :: Signal -> Signal
optimize1 = go
    where
        -- Identities
        go (Lift2 "(+)" _ (Constant 0) b) = optimize b
        go (Lift2 "(+)" _ a (Constant 0)) = optimize a
        go (Lift2 "(-)" _ a (Constant 0)) = optimize a
        go (Lift2 "(-)" _ (Constant 0) b) = optimize (negate b)

        go (Lift2 "(*)" _ (Constant 0) b) = 0
        go (Lift2 "(*)" _ a (Constant 0)) = 0
        go (Lift2 "(*)" _ (Constant 1) b) = optimize b
        go (Lift2 "(*)" _ a (Constant 1)) = optimize a

        go (Lift2 "(/)" _ (Constant 0) a) = 0
        go (Lift2 "(/)" _ a (Constant 0)) = error "optimize: Division by zero"

        -- Pre-evaluate constant expressions
        go (Lift _ f (Constant a))               = Constant $ f a
        go (Lift2 _ f (Constant a) (Constant b)) = Constant $ f a b

        -- Reordering
        -- TODO generalize to all commutative ops
        
        -- a * (x[n] * b) => x[n] * (a * b)
        -- a * (b * x[n]) => x[n] * (a * b)
        -- (x[n] * a) * b => x[n] * (a * b)
        -- (a * x[n]) * b => x[n] * (a * b)
        go noOpt@(Lift2 "(*)" f a (Lift2 "(*)" _ b c))
            | areConstant [b,c] && isVariable a     = Lift2 "(*)" f a (optimize $ Lift2 "(*)" f b c)
            | areConstant [a,c] && isVariable b     = Lift2 "(*)" f b (optimize $ Lift2 "(*)" f a c)
            | areConstant [a,b] && isVariable c     = Lift2 "(*)" f c (optimize $ Lift2 "(*)" f a b)
            | otherwise                             = noOpt

        go noOpt@(Lift2 "(*)" f (Lift2 "(*)" _ b c) a)
            | areConstant [b,c] && isVariable a     = Lift2 "(*)" f a (optimize $ Lift2 "(*)" f b c)
            | areConstant [a,c] && isVariable b     = Lift2 "(*)" f b (optimize $ Lift2 "(*)" f a c)
            | areConstant [a,b] && isVariable c     = Lift2 "(*)" f c (optimize $ Lift2 "(*)" f a b)
            | otherwise                             = noOpt


        go a = a

isVariable :: Signal -> Bool
isVariable = not . isConstant

isConstant :: Signal -> Bool
isConstant = go
    where
        go Random           = False
        go Time             = False
        go (Constant _)     = True
        go (Lift _ _ a)     = isConstant a
        go (Lift2 _ _ a b)  = isConstant a && isConstant b
        go (Input _)        = False
        go (Output _ _)     = False

areConstant :: [Signal] -> Bool
areConstant = getAll . mconcat . fmap (All . isConstant)


-- | From range (0,1) to range (-1,1)
toFull :: Num a => a -> a
toFull x = (x*2)-1

-- | From range (-1,1) to range (0,1)
toPos  :: Fractional a => a -> a
toPos x  = (x+1)/2


-- Could be more partitioneral if not due to MonoMorph..R
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

delaySec x = delay (round $ x*44100)
major freq = (sin (freq*4) + sin (freq*5) + sin (freq*6))*0.02

-- sig = delay 0 (sum $ fmap (\x -> major $ line freq*x) [1,3/2,4/5,6/7,8/9,10/11,11/12,13/14,15/16,17/18])
-- sig = delay 0 (sum $ fmap (\x -> major $ line freq*x) [1,3/2,4/5,6/7,8/9])
-- sig = sin $ line freq
-- sig = major $ line $ freq/4

sig = impulse + delaySec 0.01 impulse

-- sig = lowPass (1000+5000*sweep) 44100 0.01 6 $ random
    -- where
        -- sweep = (sin $ line (1/(10*2)) `max` 0)

freq = 440
           
numSampls = sr * secs
secs = 10
sr   = 44100 -- TODO see stateRate above




tau                 = 2 * pi
first  f (a,b)      = (f a, b)
second f (a,b)      = (a, f b)
swap (a,b)          = (b, a)
dup x               = (x, x)



