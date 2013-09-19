
{-# LANGUAGE NoMonomorphismRestriction, BangPatterns, MultiParamTypeClasses #-}

module Sound.Comb.Prim.Common (
        -- * Signal type
        Signal(..),

        -- * Inspecting signals
        isVariable,
        isConstant,
        areConstant,
        signalNodeCount,
        signalTree,
        requiredInputs,
        requiredBuses,
        requiredDelay,

        -- ** Optimization
        optimize,
        optimize1,

        -- ** Simplification
        simplify,

        -- * Signal primitives
        -- ** Basic signals
        time,
        random,
        constant,
        lift,
        lift2,
        lift',
        lift2',

        -- ** Arithmetic
        fmod,

        -- ** Special
        former,
        latter,

        -- ** Delay and feedback
        loop,
        delay,
        input,

        -- ** Utilities
        impulse,
        line,
        saw,
        saw',
        lowPass,
        biquad,
) where

import Data.Monoid
import Data.Maybe
import Data.Foldable (foldMap)
import Data.Tree
import qualified Data.Fixed as Fixed
import Sound.File.Sndfile

import Sound.Comb.Util.Part
import Sound.Comb.Util.Misc

-- |
-- Primitive signals.
--
-- Each signal represents one channel of audio. The 'Input' and 'Output' constructors
-- represent a single read or write to a global bus. Non-negative bus numbers indicate
-- global input (i.e index into the list of incoming sample sequences). Reading or writing
-- from a non-existent buffer should result in 0 and implementations must assure this either
-- by allocating zeroed memory or testing for non-existent buffer channels.
--
-- The output sequence of samples is a function of all inputs,
-- semantically @[[Double]] -> [Double]@.
--
data Signal
    -- | Elapsed time in seconds.
    = Time
    -- | A random value in the range @(-1,1)@.
    | Random
    -- | A constant value.
    | Constant Double
    -- | Lifted function with optional name.
    | Lift  String (Double -> Double) Signal                  -- string is optional name
    | Lift2 String (Double -> Double -> Double) Signal Signal -- string is optional name

    -- | Fixpoint.
    | Loop (Signal -> Signal)
    -- | Delay.
    | Delay Int Signal

    -- | Input.
    | Input Int

    -- | Output.
    | Output Int Int Signal

-- | 
-- Is this a variable (non-constant) signal?
isVariable :: Signal -> Bool
isVariable = not . isConstant

-- | 
-- Is this a constant signal?
isConstant :: Signal -> Bool
isConstant = go
    where
        go Random           = False
        go Time             = False
        go (Constant _)     = True
        go (Lift _ _ a)     = isConstant a
        go (Lift2 _ _ a b)  = isConstant a && isConstant b
        go (Input _)        = False
        go (Output _ _ _)   = False

-- | 
-- Are these all constant signals?
areConstant :: [Signal] -> Bool
areConstant = getAll . mconcat . fmap (All . isConstant)

-- | 
-- Number of nodes in the signal.
signalNodeCount :: Signal -> Int
signalNodeCount x = getSum $ foldMap (const (Sum 1)) $ signalTree x

-- | 
-- Convert a signal to a tree of names.
-- Useful for debugging optimizations etc.
signalTree :: Signal -> Tree String
signalTree = go . simplify
    where
        go Time             = Node "time" []
        go Random           = Node "random" []
        go (Constant x)     = Node (show x) []
        go (Lift n _ a)     = Node n [signalTree a]
        go (Lift2 n _ a b)  = Node n [signalTree a, signalTree b]
        go (Input c)        = Node ("input " ++ show c) []
        go (Output n c a)   = Node ("output " ++ show c ++ "[-"++show n++"]") [signalTree a]

requiredInputs :: Signal -> Int
requiredInputs = go
    where
        go Random           = 0
        go Time             = 0
        go (Constant _)     = 0
        go (Lift _ _ a)     = go a
        go (Lift2 _ _ a b)  = go a `max` go b
        go (Input c)        = if c < 0 then 0 else c + 1
        go (Output _ c a)   = (if c < 0 then 0 else c + 1) `max` go a

requiredBuses :: Signal -> Int
requiredBuses = go
    where
        go Random           = 0
        go Time             = 0
        go (Constant _)     = 0
        go (Lift _ _ a)     = go a
        go (Lift2 _ _ a b)  = go a `max` go b
        go (Input c)        = if c < 0 then unneg c + 1 else 0
        go (Output _ c a)   = (if c < 0 then unneg c + 1 else 0) `max` go a

-- | Number of required delay steps
requiredDelay :: Signal -> Int
requiredDelay = go
    where
        go Random           = 0
        go Time             = 0
        go (Constant _)     = 0
        go (Lift _ _ a)     = go a
        go (Lift2 _ _ a b)  = go a `max` go b
        go (Input _)        = 0
        go (Output n _ a)   = n `max` go a
        go _                = error "requiredDelay: Unknown signal type, perhaps you forgot simplify"

-- |
-- Optimize a signal. Only works on simplified signals.
--
optimize :: Signal -> Signal
optimize = rec . optimize1
    where
        rec (Lift n f a)     = Lift n f (optimize a)
        rec (Lift2 n f a b)  = Lift2 n f (optimize a) (optimize b)
        rec (Output n c a)   = Output n c (optimize a)
        rec a                = a

-- |
-- Perform one optimization step.
--
optimize1 :: Signal -> Signal
optimize1 = go
    where
        -- Identities
        go (Lift2 "(+)" _ (Constant 0) b) = optimize b
        go (Lift2 "(+)" _ a (Constant 0)) = optimize a
        go (Lift2 "(-)" _ a (Constant 0)) = optimize a
        -- go (Lift2 "(-)" _ (Constant 0) b) = optimize (negate b)

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

        -- TODO
        -- More advanced optimizations:
        --
        --  * Common sub-expression elimination (could be very efficient here)
        --  * Fusion (not for Faust backend!)
        --  * Join nested delays (requires a pre-simplify step)
        --  *   Or more generally: join nested input/output (possible?)

        go a = a


-- |
-- Recursively remove signal constructors not handled by 'step'.
--
-- Currently, it replaces:
--
--   * All loops with local input/outputs
--
--   * All delays with local input/output pair
--
simplify :: Signal -> Signal
simplify = go defPart
    where
        go g (Loop f)        = out $ go h (f inp)
            where
                out   = Output 1 i
                inp   = Input i
                (i, h) = first neg $ runPart g
        go g (Delay n a)        = inp `former` out
            where
                out = Output n i (go h a)
                inp = Input i
                (i, h) = first neg $ runPart g
                former  = Lift2 "former" (\x _ -> x)


        go g (Lift n f a)     = Lift n f (go g a)
        go g (Lift2 n f a b)  = Lift2 n f (go g1 a) (go g2 b) where (g1, g2) = splitPart g
        -- Note: splitPart is unnecessary if evaluation is sequential

        go g x = x

-- Evaluation order: left before right, then bottom before top
-- Delay exploits the LBR rule to evaluate the input before the delayed expression
-- Loop exploits the BBT rule to evaluate the fixed expression before the output
-- Rule for vectorized evaluation_
--     When evaluating (Output n _ a), at most n steps can be processed


--------------------------------------------------------------------------------

-- API

-- Bogus instance required by Ord
instance Eq Signal where
    (==)            = error "No (==)"
instance Ord Signal where
    compare         = error "No compare, (<) or (>)"
    max             = lift2' "max" max
    min             = lift2' "min" min
instance Num Signal where
    (+)             = lift2' "(+)" (+)
    (*)             = lift2' "(*)" (*)
    (-) = lift2' "(-)" (-)
    abs             = lift' "abs" abs
    signum          = lift' "signum" signum
    fromInteger x   = constant (fromInteger x)
instance Fractional Signal where
    recip           = lift' "recip" recip
    (/)             = lift2' "(/)" (/)
    fromRational x  = constant (fromRational x)
instance Show Signal where
    show            = drawTree . signalTree
instance Floating Signal where
    pi              = constant pi
    exp             = lift' "exp" exp
    sqrt            = lift' "sqrt" sqrt
    log             = lift' "log" log
    (**)            = lift2' "(**)" (**)
    logBase         = lift2' "logBase" logBase
    sin             = lift' "sin" sin
    tan             = lift' "tan" tan
    cos             = lift' "cos" cos
    asin            = lift' "asin" asin
    atan            = lift' "atan" atan
    acos            = lift' "acos" acos
    sinh            = lift' "sinh" sinh
    tanh            = lift' "tanh" tanh
    cosh            = lift' "cosh" cosh
    asinh           = lift' "asinh" asinh
    atanh           = lift' "atanh" atanh
    acosh           = lift' "acosh" acosh

-- | Number of seconds elapsed
time        :: Signal

-- | Random values in range (-1,1)
random      :: Signal

-- | Constant value
constant    :: Double -> Signal

-- | Read input from the given bus.
input       :: Int -> Signal


-- | Lifted unary op
lift        :: (Double -> Double) -> Signal -> Signal

-- | Lifted binary op
lift2       :: (Double -> Double -> Double) -> Signal -> Signal -> Signal

-- | Lifted unary op with a specific name.
lift'       :: String -> (Double -> Double) -> Signal -> Signal

-- | Lifted binary op with a specific name.
lift2'      :: String -> (Double -> Double -> Double) -> Signal -> Signal -> Signal

-- | Run both in given order, return the value of the first argument.
--   This is called @attach@ in Faust.
former      :: Signal -> Signal -> Signal

-- | Run both in given order, return the value of the second argument.
latter      :: Signal -> Signal -> Signal

-- | Fixpoint with implicit 1 sample delay
loop        :: (Signal -> Signal) -> Signal

-- | An n-sample delay, where n > 0
delay       :: Int -> Signal -> Signal

time    = Time
random  = Random
input   = Input
constant  = Constant
lift    = Lift "f"
lift2   = Lift2 "f"
lift'   = Lift
lift2'  = Lift2
latter  = Lift2 "latter" (\_ x -> x)
former  = Lift2 "former" (\x _ -> x)
loop    = Loop
delay   = Delay

fmod :: Signal -> Signal -> Signal
fmod = lift2' "divMod" Fixed.mod'

-- |
-- The impulse function: @1@ at time @0@, otherwise @0@.
--
impulse :: Signal
impulse = lift' "mkImp" (\x -> if x == 0 then 1 else 0) time

-- |
-- Goes from 0 to tau during @(1/x)@ seconds. Suitable for feeding an oscillator, i.e.
--
-- > sin (line 440)
--
line :: Double -> Signal
line n = time*tau*constant n

-- |
-- Goes from @-1@ to 1 during @(1/x)@ seconds. Suitable for feeding an oscillator, i.e.
--
-- > sin (line 440)
--
saw :: Double -> Signal
saw n = toFull (saw' n)

-- |
-- Goes from 0 to 1 during @(1/x)@ seconds. Suitable for feeding an oscillator, i.e.
--
-- > sin (line 440)
--
saw' :: Double -> Signal
saw' n = time*1*constant n

-- |
-- Low-pass filter, based on 'biquad'.
--
lowPass :: Signal -> Signal -> Signal -> Signal -> Signal -> Signal
lowPass fc fs q peakGain = biquad a0 a1 a2 b1 b2
    where
        (a0,a1,a2,b1,b2) = lowPassCoeffs fc fs q peakGain

        -- Where did I get this?
        -- See also http://www.musicdsp.org/files/Audio-EQ-Cookbook.txt

        lowPassCoeffs :: Floating a => a -> a -> a -> a -> (a, a, a, a, a)
        lowPassCoeffs fc fs q peakGain = (a0,a1,a2,b1,b2)
            where
                v = 10 ** abs peakGain / 20
                k = tan (pi * fc / fs)

                norm = 1 / (1+k / q+k^2)

                a0 = k^2 * norm
                a1 = 2 * a0
                a2 = a0
                b1 = 2 * (k^2 - 1) * norm
                b2 = (1 - k / q + k^2) * norm

-- |
-- A biquad filter.
--
biquad :: Signal -> Signal -> Signal -> Signal -> Signal -> Signal -> Signal
biquad b0 b1 b2 a1 a2 x = loop $ \y ->
    b0*x + b1 * delay 1 x + b2 * delay 2 x
         - a1 * delay 1 y - a2 * delay 2 y

