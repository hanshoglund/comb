
{-# LANGUAGE BangPatterns #-}

data State = State
readSamp :: Int -> State -> Double
writeSamp :: Int -> Int -> Double -> State -> State
stateTime :: State -> Double
stateRandom :: State -> (Double, State)

(readSamp, writeSamp, stateTime, stateRandom) = undefined

data Signal
    = Time
    | Random
    | Constant Double
    | Lift  String (Double -> Double) Signal                 
    | Lift2 String (Double -> Double -> Double) Signal Signal
    | Input Int
    | Output Int Int Signal


step :: Signal -> State -> (Double, State)
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

