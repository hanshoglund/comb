
{-# LANGUAGE NoMonomorphismRestriction, BangPatterns, MultiParamTypeClasses #-}

module Main where

import Sound.Comb
import Sound.Comb.Prim.Common
import Sound.Comb.Prim.Faust

main :: IO ()
main = do
    writeSignal "test.wav" sig 
    putStrLn "Finished"

delaySec x = delay (round $ x*sr)
major freq = (sin (freq*4) + sin (freq*5) + sin (freq*6))*0.02

-- sig = sweep * (sum $ fmap (\x -> major $ line freq*x) [1,3/2,4/5,6/7,8/9,10/11,11/12,13/14,15/16,17/18])
--     where
--         sweep = (sin $ line (1/(10*2)) `max` 0)
        
-- sig = delay 0 (sum $ fmap (\x -> major $ line freq*x) [1,3/2,4/5,6/7,8/9])
-- sig = sin $ line freq
-- sig = major $ line $ freq/4

-- sig = sin (line 440)*(sin (line$1/2))*0.1 + delaySec 0.1 (sin (line 440*3/5)*(sin (line$1/2))*0.1)
sig = random*0.1*sin(line $ 1/2)

-- sig = lowPass (1000+5000*sweep) sr 0.01 6 random
    -- where
        -- sweep = (sin $ line (1/(10*2)) `max` 0)

-- sig = (sum $ fmap (\x -> delaySec (x/10) impulse) [1..10])


freq = 440
           
numSampls = sr * secs
secs = 10
sr   = 44100 -- TODO see stateRate above
