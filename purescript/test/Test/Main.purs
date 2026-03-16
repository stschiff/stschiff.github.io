module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Fetch (fetch)
import Effect.Aff (launchAff_)

main :: Effect Unit
main = do
  log "🍕"
  log "You should add some tests."
