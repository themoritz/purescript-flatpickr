module Main where

import Prelude
import Flatpickr as FP
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (HTMLElement, htmlDocumentToParentNode, readHTMLElement)
import DOM.HTML.Window (document)
import DOM.Node.ParentNode (querySelector)
import Data.Either (either)
import Data.Foreign (toForeign)
import Data.JSDate (getUTCDate, jsdate, toDateTime)
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)
import Flatpickr.Config (SetOption(..), defaultConfig)
import Flatpickr.Types (DateSet(..), DateType(..), FLATPICKR)

main :: Eff (console :: CONSOLE, flatpickr :: FLATPICKR, dom :: DOM) Unit
main = do
  nullableEl <-
    window >>=
    document >>=
    (htmlDocumentToParentNode >>> querySelector "#flatpickr")
  let
    mPicker = do
      el <- toMaybe nullableEl
      either (const Nothing) Just $ runExcept $ readHTMLElement (toForeign el)
  case mPicker of
    Nothing -> pure unit
    Just picker -> example picker
  pure unit
  
example
  :: forall eff
   . HTMLElement
  -> Eff (flatpickr :: FLATPICKR, console :: CONSOLE | eff) Unit
example picker = do
  let
    someJSDate = jsdate
      { year: 2017.0
      , month: 0.0
      , day: 10.0
      , hour: 0.0
      , minute: 0.0
      , second: 0.0
      , millisecond: 0.0
      }
  fp <- FP.flatpickr picker defaultConfig
    { enableTime = true
    , disable =
      [ DateRange
        { from: DateString "2017-02-01"
        , to: DateString "2017-02-03"
        }
      , DateSingle (DateJSDate someJSDate)
      , DatePredicate \date -> pure $ getUTCDate date == 16.0
      ]
    , onReady =
      [ \_ _ _ -> log "Ready"
      ]
    , hourIncrement = 3
    }
  FP.open fp
  FP.jumpToDate (DateJSDate someJSDate) fp
  FP.setDate (DateString "2017-01-29") false fp
  FP.onChange fp \dates dateStr _ -> do
    log $ "Changed " <> dateStr
    log $ show $ map toDateTime dates
  FP.onClose fp \_ _ _ -> log "Closed"
  FP.set (SetNextArrow "->") fp
  parsed <- FP.parseDate "1985-10-18" fp
  log $ show $ toDateTime parsed
