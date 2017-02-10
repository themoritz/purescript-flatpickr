module Flatpickr
  ( flatpickr
  , changeMonth
  , clear
  , close
  , destroy
  , formatDate
  -- , jumpTodate
  , open
  , parseDate
  , redraw
  -- , set
  -- , setDate
  , toggle
  -- Hooks
  , onChange
  , onOpen
  , onClose
  , onMonthChange
  , onYearChange
  , onReady
  , onValueUpdate
  , onDayCreate
  ) where

import Prelude
import DOM.HTML.Types (HTMLElement)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.JSDate (JSDate)
import Flatpickr.Types (Config, Flatpickr, FlatEff)

foreign import flatpickrImpl :: forall r eff. Fn2 HTMLElement (Config r) (FlatEff eff Flatpickr)

flatpickr :: forall r eff. HTMLElement -> Config r -> FlatEff eff Flatpickr
flatpickr = runFn2 flatpickrImpl

foreign import changeMonthImpl :: forall eff. Fn3 Int Boolean Flatpickr (FlatEff eff Unit)

changeMonth :: forall eff. Int -> Boolean -> Flatpickr -> FlatEff eff Unit
changeMonth = runFn3 changeMonthImpl

foreign import clear :: forall eff. Flatpickr -> FlatEff eff Unit

foreign import close :: forall eff. Flatpickr -> FlatEff eff Unit

foreign import destroy :: forall eff. Flatpickr -> FlatEff eff Unit

foreign import formatDateImpl :: forall eff. Fn3 String JSDate Flatpickr (FlatEff eff String)

formatDate :: forall eff. String -> JSDate -> Flatpickr -> FlatEff eff String
formatDate = runFn3 formatDateImpl

-- TODO: jumpToDate

foreign import open :: forall eff. Flatpickr -> FlatEff eff Unit

foreign import parseDateImpl :: forall eff. Fn2 String Flatpickr (FlatEff eff String)

parseDate :: forall eff. String -> Flatpickr -> FlatEff eff String
parseDate = runFn2 parseDateImpl

foreign import redraw :: forall eff. Flatpickr -> FlatEff eff Unit

-- TODO: set

-- TODO: setDate

foreign import toggle :: forall eff. Flatpickr -> FlatEff eff Unit

-- Hooks

type Hook eff = Array JSDate -> String -> Flatpickr -> FlatEff eff Unit

foreign import hookImpl :: forall eff. Fn3 String Flatpickr (Hook eff) (FlatEff eff Unit)

onChange :: forall eff. Flatpickr -> Hook eff -> FlatEff eff Unit
onChange = runFn3 hookImpl "onChange"

onOpen :: forall eff. Flatpickr -> Hook eff -> FlatEff eff Unit
onOpen = runFn3 hookImpl "onOpen"

onClose :: forall eff. Flatpickr -> Hook eff -> FlatEff eff Unit
onClose = runFn3 hookImpl "onClose"

onMonthChange :: forall eff. Flatpickr -> Hook eff -> FlatEff eff Unit
onMonthChange = runFn3 hookImpl "onMonthChange"

onYearChange :: forall eff. Flatpickr -> Hook eff -> FlatEff eff Unit
onYearChange = runFn3 hookImpl "onYearChange"

onReady :: forall eff. Flatpickr -> Hook eff -> FlatEff eff Unit
onReady = runFn3 hookImpl "onReady"

onValueUpdate :: forall eff. Flatpickr -> Hook eff -> FlatEff eff Unit
onValueUpdate = runFn3 hookImpl "onValueUpdate"

onDayCreate :: forall eff. Flatpickr -> Hook eff -> FlatEff eff Unit
onDayCreate = runFn3 hookImpl "onDayCreate"
