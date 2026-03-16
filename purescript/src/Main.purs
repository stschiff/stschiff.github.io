module Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Fetch (fetch)
import Effect.Aff (Aff, launchAff_)

main :: Effect Unit
main = do
  log "🍝"
  launchAff_ $ do
    r <- get_toots "109301761847534867"
    log r
 
get_toots :: String -> Aff String
get_toots account_id = do
  let url = "https://ecoevo.social/api/v1/accounts/" <> account_id <>
        "/statuses?exclude_replies=true&exclude_reblogs=true"
  fetch url {} >>= _.text