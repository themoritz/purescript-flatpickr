module Flatpickr.Types where

import Prelude
import Control.Monad.Eff (Eff)
import Data.JSDate (JSDate)

foreign import data FLATPICKR :: !

data Flatpickr

data DateType
  = DateString String
  | DateJSDate JSDate
  | DateNull

type DateRange =
  { from :: DateType
  , to :: DateType
  }

data DateSet eff
  = DateSingle DateType
  | DateRange DateRange
  | DatePredicate (JSDate -> Eff eff Boolean)

type FlatEff eff = Eff (flatpickr :: FLATPICKR | eff)

type Hook eff = Array JSDate -> String -> Flatpickr -> FlatEff eff Unit
