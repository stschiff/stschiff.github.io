module Main where

import Prelude

-- import Data.Argonaut (Json)
import Data.Array (length, filter)
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Fetch (fetch)
import Fetch.Argonaut.Json (fromJson)
import Effect.Aff (Aff, launchAff_)
import Web.DOM.Document (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)

type Toot = {
  in_reply_to_id :: Maybe String,
  media_attachments :: Array ({
    url :: String,
    preview_url :: String
  }),
  created_at :: String,
  account :: {
    url :: String,
    display_name :: String,
    username :: String
  },
  content :: String,
  url :: String
}

main :: Effect Unit
main = do
  log "🍝"
  r <- launchAff_ $ get_toots "109301761847534867"
  logShow r
  log $ "Found " <> show (length r) <> " toots"
  win <- window
  doc <- toDocument $ document win
  maybeEl <- getElementById "toot-list" $ toNonElementParentNode doc
  case maybeEl of
    Nothing -> log "Could not find element with id 'toot-list'"
    Just toot_list -> do
      for r $ \toot -> do
        let el = createElement "li" doc
        el.innerHTML = toot.content
        pure ()
        appendChild (toNode el) (toNode toot_list)


get_toots :: String -> Aff (Array Toot)
get_toots account_id = do
  let url = "https://ecoevo.social/api/v1/accounts/" <> account_id <>
        "/statuses?exclude_replies=true&exclude_reblogs=true"
  { json, ok } <- fetch url {}
  toots <- if ok
    then do
      jsonParsed <- fromJson json
      pure jsonParsed
    else do
      log $ "Error fetching toots for account: " <> account_id
      pure []
  pure $ filter (\toot -> toot.in_reply_to_id == Nothing) toots
