module Flatpickr
  ( flatpickr
  , changeMonth
  , clear
  , close
  , destroy
  , formatDate
  , jumpToDate
  , open
  , parseDate
  , redraw
  , set
  , setDate
  , toggle
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
import Control.Monad.Eff (Eff)
import DOM.HTML.Types (HTMLElement)
import Data.Foreign (Foreign, toForeign)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.JSDate (JSDate)
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Data.Tuple (Tuple(..))
import Flatpickr.Config (Config, Mode(..), SetOption(..))
import Flatpickr.Types (DateRange, DateSet(..), DateType(..), FlatEff, Flatpickr, Hook)

modeToForeign :: Mode -> Foreign
modeToForeign mode = toForeign $ case mode of
  Single   -> "single"
  Multiple -> "multiple"
  Range    -> "range"

dateTypeToForeign :: DateType -> Foreign
dateTypeToForeign = case _ of
  DateString s    -> toForeign s
  DateJSDate date -> toForeign date
  DateNull        -> toForeign $ toNullable Nothing

dateRangeToForeign :: DateRange -> Foreign
dateRangeToForeign r = toForeign
  { from: dateTypeToForeign r.from
  , to: dateTypeToForeign r.to
  }

foreign import runPredicateImpl :: forall eff. (JSDate -> Eff eff Boolean) -> Foreign

dateSetToForeign :: forall eff. DateSet eff -> Foreign
dateSetToForeign = case _ of
  DateSingle i    -> dateTypeToForeign i
  DateRange r     -> dateRangeToForeign r
  DatePredicate p -> runPredicateImpl p

foreign import runHookImpl :: forall eff. Hook eff -> Foreign

configToForeign :: forall eff. Config eff -> Foreign
configToForeign c = toForeign $ c
  { appendTo    = toNullable c.appendTo
  , defaultDate = dateTypeToForeign c.defaultDate
  , disable     = map dateSetToForeign c.disable
  , enable      = map dateSetToForeign c.enable
  , maxDate     = dateTypeToForeign c.maxDate
  , minDate     = dateTypeToForeign c.minDate
  , mode        = modeToForeign c.mode
  , onChange    = map runHookImpl c.onChange
  , onClose     = map runHookImpl c.onClose
  , onOpen      = map runHookImpl c.onOpen
  , onReady     = map runHookImpl c.onReady
  , parseDate   = toNullable c.parseDate
  }

foreign import flatpickrImpl :: forall eff. Fn2 HTMLElement Foreign (FlatEff eff Flatpickr)

flatpickr :: forall eff. HTMLElement -> Config eff -> FlatEff eff Flatpickr
flatpickr el cfg = runFn2 flatpickrImpl el (configToForeign cfg)

foreign import changeMonthImpl :: forall eff. Fn3 Int Boolean Flatpickr (FlatEff eff Unit)

changeMonth :: forall eff. Int -> Boolean -> Flatpickr -> FlatEff eff Unit
changeMonth = runFn3 changeMonthImpl

foreign import clear :: forall eff. Flatpickr -> FlatEff eff Unit

foreign import close :: forall eff. Flatpickr -> FlatEff eff Unit

foreign import destroy :: forall eff. Flatpickr -> FlatEff eff Unit

foreign import formatDateImpl :: forall eff. Fn3 String JSDate Flatpickr (FlatEff eff String)

formatDate :: forall eff. String -> JSDate -> Flatpickr -> FlatEff eff String
formatDate = runFn3 formatDateImpl

foreign import jumpToDateImpl :: forall eff. Fn2 Foreign Flatpickr (FlatEff eff String)

jumpToDate :: forall eff. DateType -> Flatpickr -> FlatEff eff String
jumpToDate date = runFn2 jumpToDateImpl (dateTypeToForeign date)

foreign import open :: forall eff. Flatpickr -> FlatEff eff Unit

foreign import parseDateImpl :: forall eff. Fn2 String Flatpickr (FlatEff eff JSDate)

parseDate :: forall eff. String -> Flatpickr -> FlatEff eff JSDate
parseDate = runFn2 parseDateImpl

foreign import redraw :: forall eff. Flatpickr -> FlatEff eff Unit

optionToKeyValue :: forall eff. SetOption eff -> Tuple String Foreign
optionToKeyValue = case _ of
  SetAltFormat             v -> Tuple "altFormat" (toForeign v)
  SetAltInput              v -> Tuple "altInput" (toForeign v)
  SetAltInputClass         v -> Tuple "altInputclass" (toForeign v)
  SetAllowInput            v -> Tuple "allowInput" (toForeign v)
  SetClickOpens            v -> Tuple "clickOpens" (toForeign v)
  SetDateFormat            v -> Tuple "dateFormat" (toForeign v)
  SetDefaultDate           v -> Tuple "defaultDate" (dateTypeToForeign v)
  SetDisable               v -> Tuple "disable" (toForeign $ map dateSetToForeign v)
  SetDisableMobile         v -> Tuple "disableMobile" (toForeign v)
  SetEnable                v -> Tuple "enable" (toForeign $ map dateSetToForeign v)
  SetEnableTime            v -> Tuple "enableTime" (toForeign v)
  SetEnableSeconds         v -> Tuple "enableSeconds" (toForeign v)
  SetHourIncrement         v -> Tuple "hourIncrement" (toForeign v)
  SetInline                v -> Tuple "inline" (toForeign v)
  SetMaxDate               v -> Tuple "maxDate" (dateTypeToForeign v)
  SetMinDate               v -> Tuple "minDate" (dateTypeToForeign v)
  SetMinuteIncrement       v -> Tuple "minuteIncrement" (toForeign v)
  SetMode                  v -> Tuple "mode" (modeToForeign v)
  SetNextArrow             v -> Tuple "nextArrow" (toForeign v)
  SetNoCalendar            v -> Tuple "noCalendar" (toForeign v)
  SetOnChange              v -> Tuple "onChange" (toForeign $ map runHookImpl v)
  SetOnClose               v -> Tuple "onClose" (toForeign $ map runHookImpl v)
  SetOnOpen                v -> Tuple "onOpen" (toForeign $ map runHookImpl v)
  SetOnReady               v -> Tuple "onReady" (toForeign $ map runHookImpl v)
  SetParseDate             v -> Tuple "parseDate" (toForeign $ toNullable v)
  SetPrevArrow             v -> Tuple "prevArrow" (toForeign v)
  SetShorthandCurrentMonth v -> Tuple "shorthandCurrentMonth" (toForeign v)
  SetStatic                v -> Tuple "static" (toForeign v)
  SetTime_24hr             v -> Tuple "time_24hr" (toForeign v)
  SetUtc                   v -> Tuple "utc" (toForeign v)
  SetWeekNumbers           v -> Tuple "weekNumbers" (toForeign v)
  SetWrap                  v -> Tuple "wrap" (toForeign v)

foreign import setImpl :: forall eff. Fn3 String Foreign Flatpickr (FlatEff eff Unit)

-- | Does not seem to work (due to bug in flatpickr?) currently. Use `Config`
-- | instead if possible.
set :: forall eff. SetOption eff -> Flatpickr -> FlatEff eff Unit
set option inst =
  case optionToKeyValue option of
    Tuple key value -> runFn3 setImpl key value inst

foreign import setDateImpl :: forall eff. Fn3 Foreign Boolean Flatpickr (FlatEff eff String)

setDate :: forall eff. DateType -> Boolean -> Flatpickr -> FlatEff eff String
setDate date = runFn3 setDateImpl (dateTypeToForeign date)

foreign import toggle :: forall eff. Flatpickr -> FlatEff eff Unit

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
