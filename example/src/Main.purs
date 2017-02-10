module Main where

import Prelude
import Flatpickr as FP
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToParentNode, readHTMLElement)
import DOM.HTML.Window (document)
import DOM.Node.ParentNode (querySelector)
import Data.Either (either)
import Data.Foreign (toForeign)
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)
import Flatpickr.Types (FLATPICKR)

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
    Just picker -> do
      fp <- FP.flatpickr picker
        { enableTime: true
        }
      FP.open fp
      FP.onChange fp \dates dateStr _ -> log $ "Changed " <> dateStr
      FP.onClose fp \_ _ _ -> log "Closed"
      pure unit
  pure unit
  
