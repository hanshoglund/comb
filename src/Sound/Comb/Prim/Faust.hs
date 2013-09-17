

module Sound.Comb.Prim.Faust where

import System.Process
import Sound.Comb.Prim.Common

signalToFaust :: Signal -> String
signalToFaust a = prefix ++ "process = " ++ go 0 a ++ ";\n\n"
    where
        go v Time             = "time_"
        go v Random           = "random_"
        go v (Constant x)     = show x

        go v (Loop f)         = "((\\(" ++ varName v ++ ").(" ++ go (v+1) (f $ Input v) ++ ")) ~ _)"

        go v (Delay n a)      = go v a ++ " @ " ++ show n
        go v (Lift n f a)     = func n ++ "(" ++ go v a ++ ")"
        go v (Lift2 n f a b)  = func n ++ "(" ++ go v a ++ "," ++ go v b ++ ")"

        go v (Input c)         = varName c
        go v (Output n c a)    = go v a

        varName 0 = "x"
        varName 1 = "y"
        varName 2 = "z"
        varName n = "v" ++ show n
        
        func ['(',o1,')']    = [o1]
        func ['(',o1,o2,')'] = [o1,o2]
        func a               = a
        
        prefix = "time_ = (+(1) ~ _); \n\n"

        -- Lift  String (Double -> Double) Signal                  -- string is optional name
        -- Lift2 String (Double -> Double -> Double) Signal Signal -- string is optional name


drawSignalFaust :: Signal -> IO ()
drawSignalFaust a = do
    writeFile "test.dsp" (signalToFaust a)
    system "faust2svg test.dsp"
    -- system "open test-svg/process.svg"
    return ()