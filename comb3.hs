
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

type Signal = [Double]

-- instance Eq Signal where
    -- (==) = error "No (==)"
instance Ord Signal where
    compare = error "No compare"
    max = lift2 max
    min = lift2 min
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


time    :: Signal
input   :: Int -> Signal
signal  :: Double -> Signal
lift    :: (Double -> Double) -> Signal -> Signal
lift2   :: (Double -> Double -> Double) -> Signal -> Signal -> Signal
loop    :: (Signal -> Signal) -> Signal
delay   :: Signal -> Signal
-- delay or loop' :: (S -> S -> (S,S)) -> S -> S must be primitive
-- delay = loop' (\a b -> (b, a))
time    = [1..]
input _ = fmap (/ 10) [6,1,-2,3,5,-6,7,1,-2,6,4]
signal  = repeat
lift    = fmap
lift2   = zipWith
loop f  = f (loop f)
delay   = (0 :)




main = putSignal $ sin (time/5)
    


-- loop (\y -> i*0.5 + x*0.5)







runSignal :: Signal -> [Double]
runSignal = id

putSignal :: Signal -> IO ()
putSignal a = mapM_ (putStrLn.toBars) $ take 60 $ runSignal a

-- | From range (0,1) to range (-1,1)
toFull :: Num a => a -> a
toFull x = (x*2)-1

-- | From range (-1,1) to range (0,1)
toPos  :: Fractional a => a -> a
toPos x  = (x+1)/2

-- | View as bars if in range (-1,1)
toBars :: RealFrac a => a -> String
toBars x = let n = round (toPos x * fromIntegral width) in
    if n > width || n < 0
        then replicate (width + 1) ' ' ++ "|"
        else replicate n ' ' ++ "." ++ replicate (width-n) ' ' ++ "|"
    where
        width = 80


tau                 = 2 * pi
first  f (a,b)      = (f a, b)
second f (a,b)      = (a, f b)
fromJust (Just x)   = x
dup x               = (x, x)
