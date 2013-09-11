
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
import Data.Tree
import Sound.PortAudio
import Sound.PortAudio.Base(PaStreamCallbackTimeInfo)
import Control.Concurrent (threadDelay)


--  A signal is a function of inputs and time over some local state
--  Note that input/outputs may include global buffers
--  TODO strictness, turn networks on and off (stepping)
--  TODO higher-order, signals of signals (switching)
type Time   = Int
data State  = State {
        stateInputs     :: [Double],        -- current input values
        -- stateBuses      :: [Signal],        -- current buses
        stateCount      :: Int,             -- processed samples
        stateRate       :: Double,          -- samples per second

        stateLocalBufs  :: [Double],        -- local buffers
        stateLocalBuf   :: Int,             -- node of current local buffer
        stateLocalBufInc :: Int             -- difference between left and right branch 
    }
    deriving (Show)
instance Default State where
    def = State 
        [] {-[]-} 0 10 
        [] 0 1
defState = (def::State)
    
-- instance Monoid State where

readBuf  :: State -> Double
writeBuf :: Double -> State -> State
readBuf = undefined
writeBuf = undefined

left :: State -> State
right :: State -> State
unleft :: State -> State
unright :: State -> State
left = id
unleft = id
right x = x { stateLocalBuf = stateLocalBuf x + stateLocalBufInc x }
unright x = x { stateLocalBuf = stateLocalBuf x - stateLocalBufInc x }


down :: State -> State
up   :: State -> State
down x = right   $ x { stateLocalBufInc = stateLocalBufInc x * 2 }
up   x = unright $ x { stateLocalBufInc = stateLocalBufInc x `div` 2 }

move n = compTimes n unright right

walkStateTree :: Tree State -> Tree State
walkStateTree (Node x xs) = Node x (fmap2 down $ mapWithIndex (\n x -> x) xs)


mapWithIndex f = zipWith f [0..]
fmap2 = fmap . fmap
    
showStateNodes = putStrLn $ drawTree $ fmap (show . stateLocalBuf) $ 
    Node defState [
        Node (left $ down def) [], Node (right $ down def) []
    ]




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

time    :: Signal
input   :: Int -> Signal
signal  :: Double -> Signal
lift    :: (Double -> Double) -> Signal -> Signal
lift2   :: (Double -> Double -> Double) -> Signal -> Signal -> Signal
loop    :: (Double -> Double -> (Double,Double)) -> Signal -> Signal
delay   :: Signal -> Signal
time        = Signal $ \s -> (s, fromIntegral (stateCount s) / stateRate s)
input n     = Signal $ \s -> (s, stateInputs s !! n)
signal x    = Signal $ \s -> (s, x)
lift f a    = Signal $ \s -> let 
    (sa, xa) = (getSignal a) s 
    in (sa, f xa)
lift2 f a b = Signal $ \s -> let
    (sa, xa) = first unleft $ (getSignal a) (left s)
    (sb, xb) = first unright $ (getSignal b) (right sa)
    in (sb, f xa xb)
    
loop f a = Signal $ \s -> let
    (sa, xa) = (getSignal a) s
    xb       = readBuf s
    (xc,xd)  = f xb xa
    sc       = writeBuf xc sa
    in (sc, xd)
    
delay       = loop $ \o n -> (n, o)

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

-- > compTimes n pred succ 0 = n
compTimes :: Int -> (a -> a) -> (a -> a) -> (a -> a)
compTimes n neg pos 
    | n <  0 = foldr (.) id (replicate (negate n) neg)
    | n >= 0 = foldr (.) id (replicate n pos)



