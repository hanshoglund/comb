
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
-- import System.Random
import Data.Functor.Contravariant
import Foreign.Ptr
import Foreign.C.Types
import Control.Applicative
import Control.Monad
import Data.List (mapAccumL)
import Foreign.Storable
import Data.List (transpose, unfoldr)
import Data.Tree
import Sound.PortAudio
import Sound.PortAudio.Base(PaStreamCallbackTimeInfo)
import Control.Concurrent (threadDelay)


-- generate unique sets
-- Laws:

-- > gen new = [1..]
-- > genAll a `merge` genAll b = genAll c   iff   (a,b) = split c
-- > genAll A is distinct from genAll b

type Gen a = (a,a) -- offset, diff

new   :: Num a => Gen a
gen   :: Num a => Gen a -> (Gen a, a)
split :: Num a => Gen a -> (Gen a, Gen a)

genAll :: Num a => Gen a -> [a]
genAll g = let
    (g2,x) = gen g
    in x : genAll g2

skip = fst . gen
next = snd . gen

new           = (0,1)
gen     (o,d) = ((o+d,d), o)
split   (o,d) = ((o,d*2), (d,d*2))




--  A signal is a function of inputs and time over some local state
--  Note that input/outputs may include global buffers
--  TODO higher-order, signals of signals (switching)
type Time   = Int
data State  = State {
        stateInputs     :: [Double],        -- current input values
        -- stateBuses      :: [Signal],        -- current buses
        stateCount      :: Int,             -- processed samples
        stateRate       :: Double           -- samples per second
    }
    deriving (Show)

instance Default State where
    def = State 
        [] {-[]-} 0 10 
defState = (def::State)
    

data Signal
    = Time
    | Constant Double
    | Lift (Double -> Double) Signal
    | Lift2 (Double -> Double -> Double) Signal Signal

    | Loop (Signal -> Signal)
    | Delay Signal

    | Input Int 
    -- >= 0 means real (global) input
    -- <  0 means local (feedback) input
    | Output Int Signal
    -- only used for feedback for now

instance Eq Signal where
    (==) = error "No (==)"
instance Ord Signal where
    compare = error "No compare"
    max = lift2 max
    min = lift2 min
instance Num Signal where
    (+) = lift2 (+)
    (*) = lift2 (*)    
    (-) = lift2 (-)
    abs           = lift abs
    signum        = lift signum
    fromInteger x = signal (fromInteger x)
instance Fractional Signal where
    recip = lift recip
    fromRational x = signal (fromRational x)
instance Show Signal where
    show = drawTree . signalTree
instance Floating Signal where
    pi = signal pi
    exp = lift exp
    sqrt = lift sqrt
    log = lift log
    (**) = lift2 (**)
    logBase = lift2 logBase
    sin = lift sin
    tan = lift tan
    cos = lift cos
    asin = lift asin
    atan = lift atan
    acos = lift acos
    sinh = lift sinh
    tanh = lift tanh
    cosh = lift cosh
    asinh = lift asinh
    atanh = lift atanh
    acosh = lift acosh


signalTree :: Signal -> Tree String
signalTree = go . simplify
    where
        go Time             = Node "Time" []
        go (Constant x)     = Node (show x) []
        go (Lift _ a)       = Node "f" [signalTree a]
        go (Lift2 _ a b)    = Node "f" [signalTree a, signalTree b]
        go (Input n)        = Node ("In: " ++ show n) []
        go (Output n a)     = Node ("Out: " ++ show n) [signalTree a] 

time    :: Signal
input   :: Int -> Signal
signal  :: Double -> Signal
lift    :: (Double -> Double) -> Signal -> Signal
lift2   :: (Double -> Double -> Double) -> Signal -> Signal -> Signal
both    :: Signal -> Signal -> Signal -- run both, return second
loop    :: (Signal -> Signal) -> Signal
delay   :: Signal -> Signal
time    = Time
input   = Input
signal  = Constant
lift    = Lift
lift2   = Lift2
both    = Lift2 (\_ x -> x)
loop    = Loop
delay   = Delay

-- Replace:
--   * All loops with local input/outputs
simplify :: Signal -> Signal
simplify = go new
    where
        go g (Loop sf)      = Output (neg $ next g) $ go (skip g) $ sf $ Input (neg $ next g)
        go g (Delay a)      = Output (neg $ next g) a `both` Input (neg $ next g)
        go g (Lift f a)     = Lift f (go g a)
        go g (Lift2 f a b)  = Lift2 f (go g1 a) (go g2 b) where (g1, g2) = split g
        -- Note: split is unnecessary if evaluation is sequential
        go g x = x                                     
        neg x = negate (x + 1)

put :: Signal -> IO ()
put a = mapM_ (putStrLn.toBars) $ take 60 $ run a

run :: Signal -> [Double]
run a = unfoldr (Just . fmap f . swap . step a) defState
    where
        f x = x { stateCount = stateCount x + 1 }

-- Run a signal over a state
-- Note that the signal is the first argument, which is usually applied once
-- The resulting (State -> (State, Double)) function is then unfolded to yield the outputs
-- Think of the repeated s application as 'run time'
step :: Signal -> State -> (State, Double)
step = go . simplify
    where
        go Time s           = (s, fromIntegral (stateCount s) / stateRate s) 
        go (Constant x) s   = (s, x)
 
        go (Lift f a) s     = let 
            (sa, xa) = a `step` s 
            in (sa, f xa)
        go (Lift2 f a b) s  = let
            (sa, xa) = a `step` s
            (sb, xb) = b `step` sa
            in (sb, f xa xb)      
 
        go (Input n) s      = (s, stateInputs s !! n) -- TODO handle negative
        go (Output n a) s   = first id $ a `step` s -- TODO modify s


-- | From range (0,1) to range (-1,1)
toFull :: Num a => a -> a
toFull x = (x*2)-1

-- | From range (-1,1) to range (0,1)
toPos  :: Fractional a => a -> a
toPos x  = (x+1)/2


-- | View as bars if in range (-1,1)
toBars :: RealFrac a => a -> String
toBars x = let n = round (toPos x * width) in
    if n > width || n < 0
        then replicate (width+1) ' ' ++ "|"
        else replicate n ' ' ++ "." ++ replicate (width-n) ' ' ++ "|"
    where 
        width = 80



tau                 = 2 * pi
first  f (a,b)      = (f a, b)
second f (a,b)      = (a, f b)
swap (a,b)          = (b, a)
cast'               = fromJust . cast
fromJust (Just x)   = x
dup x               = (x, x)



