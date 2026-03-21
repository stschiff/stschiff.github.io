module Main where
  

import NewsApp as NewsApp

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Halogen.Aff (runHalogenAff)
import Halogen.Aff.Util (selectElement)
import Halogen.VDom.Driver (runUI)
import Prelude
import Web.DOM.ParentNode (QuerySelector(..))

newsAppMain :: String -> Effect Unit
newsAppMain selector = runHalogenAff do
  mdiv <- selectElement $ QuerySelector selector
  case mdiv of
    Nothing -> pure unit
    Just div -> do
      void $ runUI NewsApp.component unit div
