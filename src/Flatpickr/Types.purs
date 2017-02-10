module Flatpickr.Types where

import Control.Monad.Eff (Eff)

foreign import data FLATPICKR :: !

data Flatpickr

type FlatEff eff = Eff (flatpickr :: FLATPICKR | eff)

type Config r = r
