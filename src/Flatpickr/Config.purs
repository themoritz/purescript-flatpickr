module Flatpickr.Config where

import Control.Monad.Eff (Eff)
import DOM.HTML.Types (HTMLElement)
import Data.JSDate (JSDate)
import Data.Maybe (Maybe(..))
import Flatpickr.Types (DateType(..), DateSet, Hook)

data Mode
  = Single
  | Multiple
  | Range

type Config eff =
  { altFormat             :: String
  , altInput              :: Boolean
  , altInputClass         :: String
  , allowInput            :: Boolean
  , appendTo              :: Maybe HTMLElement
  , clickOpens            :: Boolean
  , dateFormat            :: String
  , defaultDate           :: DateType
  , disable               :: Array (DateSet eff)
  , disableMobile         :: Boolean
  , enable                :: Array (DateSet eff)
  , enableTime            :: Boolean
  , enableSeconds         :: Boolean
  , hourIncrement         :: Int
  , inline                :: Boolean
  , maxDate               :: DateType
  , minDate               :: DateType
  , minuteIncrement       :: Int
  , mode                  :: Mode
  , nextArrow             :: String
  , noCalendar            :: Boolean
  , onChange              :: Array (Hook eff)
  , onClose               :: Array (Hook eff)
  , onOpen                :: Array (Hook eff)
  , onReady               :: Array (Hook eff)
  , parseDate             :: Maybe (String -> Eff eff JSDate)
  , prevArrow             :: String
  , shorthandCurrentMonth :: Boolean
  , static                :: Boolean
  , time_24hr             :: Boolean
  , utc                   :: Boolean
  , weekNumbers           :: Boolean
  , wrap                  :: Boolean
  }

defaultConfig :: forall eff. Config eff
defaultConfig =
  { altFormat             : "F j, Y"
  , altInput              : false
  , altInputClass         : ""
  , allowInput            : false
  , appendTo              : Nothing
  , clickOpens            : true
  , dateFormat            : "Y-m-d"
  , defaultDate           : DateNull
  , disable               : []
  , disableMobile         : false
  , enable                : []
  , enableTime            : false
  , enableSeconds         : false
  , hourIncrement         : 1
  , inline                : false
  , maxDate               : DateNull
  , minDate               : DateNull
  , minuteIncrement       : 5
  , mode                  : Single
  , nextArrow             : "<svg version='1.1' xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' viewBox='0 0 17 17'><g></g><path d='M13.207 8.472l-7.854 7.854-0.707-0.707 7.146-7.146-7.146-7.148 0.707-0.707 7.854 7.854z' /></svg>"
  , noCalendar            : false
  , onChange              : []
  , onClose               : []
  , onOpen                : []
  , onReady               : []
  , parseDate             : Nothing
  , prevArrow             : "<svg version='1.1' xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' viewBox='0 0 17 17'><g></g><path d='M5.207 8.471l7.146 7.147-0.707 0.707-7.853-7.854 7.854-7.853 0.707 0.707-7.147 7.146z' /></svg>"
  , shorthandCurrentMonth : false
  , static                : false
  , time_24hr             : false
  , utc                   : false
  , weekNumbers           : false
  , wrap                  : false
  }
  
data SetOption (eff :: # !)
  = SetAltFormat             String
  | SetAltInput              Boolean
  | SetAltInputClass         String
  | SetAllowInput            Boolean
  | SetClickOpens            Boolean
  | SetDateFormat            String
  | SetDefaultDate           DateType
  | SetDisable               (Array (DateSet eff))
  | SetDisableMobile         Boolean
  | SetEnable                (Array (DateSet eff))
  | SetEnableTime            Boolean
  | SetEnableSeconds         Boolean
  | SetHourIncrement         Int
  | SetInline                Boolean
  | SetMaxDate               DateType
  | SetMinDate               DateType
  | SetMinuteIncrement       Int
  | SetMode                  Mode
  | SetNextArrow             String
  | SetNoCalendar            Boolean
  | SetOnChange              (Array (Hook eff))
  | SetOnClose               (Array (Hook eff))
  | SetOnOpen                (Array (Hook eff))
  | SetOnReady               (Array (Hook eff))
  | SetParseDate             (Maybe (String -> Eff eff JSDate))
  | SetPrevArrow             String
  | SetShorthandCurrentMonth Boolean
  | SetStatic                Boolean
  | SetTime_24hr             Boolean
  | SetUtc                   Boolean
  | SetWeekNumbers           Boolean
  | SetWrap                  Boolean
