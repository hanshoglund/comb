
module Sound.Comb.Util.Part where

-- | 
-- Represents a partition of the set of positive integers, here generalized to work with
-- arbitrary 'Num' types.
-- 
-- > runPart defPart = [1..]
-- > sort (runPartAll a ++ runPartAll b) = runPartAll c, iff (a,b) = splitPart c
-- > nub (runPartAll a ++ runPartAll b) = runPartAll a ++ runPartAll b
-- 
newtype Part a = Part { getPart ::(a,a) } -- offset, diff

defPart     :: Num a => Part a
runPart     :: Num a => Part a -> (a, Part a)
splitPart   :: Num a => Part a -> (Part a, Part a)
nextP       :: Num a => Part a -> a
skipP       :: Num a => Part a -> Part a
runPartAll  :: Num a => Part a -> [a]

defPart                 = Part (0,1)
runPart   (Part (o,d))  = (o, Part (o+d,d))
splitPart (Part (o,d))  = (Part (o,d*2), Part (d,d*2))
nextP                   = fst . runPart
skipP                   = snd . runPart
runPartAll g    = let
    (x,g2) = runPart g
    in x : runPartAll g2

