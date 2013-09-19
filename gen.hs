
{-# LANGUAGE BangPatterns, GADTs #-}

data State a = State
readSamp    :: Int -> State a -> a
writeSamp   :: Int -> Int -> a -> State a -> State a
stateTime   :: State a -> a
stateRandom :: State a -> (a, State a)

(readSamp, writeSamp, stateTime, stateRandom) = undefined

data Signal a where
    Time        :: Signal Double
    Random      :: Signal Double
    Constant    :: a -> Signal a
    Lift        :: String -> (a -> b) -> Signal a -> Signal b
    Lift2       :: String -> (a -> b -> c) -> Signal a -> Signal b -> Signal c
    Input       :: Int -> Signal Double
    Output      :: Int -> Int -> Signal Double -> Signal Double


step :: Signal a -> State Double -> (a, State Double)
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

