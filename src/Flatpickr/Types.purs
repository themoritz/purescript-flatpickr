module Flatpickr.Types where

import Control.Monad.Eff (Eff)

foreign import data FLATPICKR :: !

data Flatpickr

type FlatEff eff = Eff (flatpickr :: FLATPICKR | eff)

-- | At the moment this is just a record that can be filled with the [available
-- | options](https://chmln.github.io/flatpickr/#options).
type Config r = r
