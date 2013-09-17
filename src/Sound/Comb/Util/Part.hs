
module Sound.Comb.Util.Part where

-- | 
-- 'Part Integer' represents a partition of the set of positive integers.
-- 
-- > runPart newPart = [1..]
-- > runPartAll a `merge` runPartAll b = runPartAll c   iff   (a,b) = splitPart c
-- > runPartAll A is distinct from runPartAll b
-- 
type Part a = (a,a) -- offset, diff

newPart     :: Num a => Part a
runPart     :: Num a => Part a -> (Part a, a)
splitPart   :: Num a => Part a -> (Part a, Part a)
nextP       :: Num a => Part a -> a
skipP       :: Num a => Part a -> Part a
runPartAll  :: Num a => Part a -> [a]

newPart         = (0,1)
runPart (o,d)   = ((o+d,d), o)
splitPart (o,d) = ((o,d*2), (d,d*2))
nextP           = snd . runPart
skipP           = fst . runPart
runPartAll g    = let
    (g2,x) = runPart g
    in x : runPartAll g2

