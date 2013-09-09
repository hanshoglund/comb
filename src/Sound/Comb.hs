
{-# LANGUAGE TypeOperators, DeriveFunctor, GADTs #-}

module Sound.Comb (
    Sink,
    Source,
    Task,
    run,


    ) where


import Data.Int
import Data.Functor.Contravariant
import Foreign.Ptr
import Foreign.C.Types
import Control.Applicative
import Control.Monad
import Foreign.Storable
import Sound.PortAudio
import Control.Concurrent (threadDelay)

-- TODO use non-default audio device

