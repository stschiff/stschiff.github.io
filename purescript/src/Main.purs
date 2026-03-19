module Main where

import Prelude

import Data.Array (length, filter)
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Fetch (fetch)
import Fetch.Argonaut.Json (fromJson)
import Effect.Aff (Aff, launchAff_)
import Halogen as H
import Halogen.Aff (runHalogenAff)
import Halogen.Aff.Util (selectElement)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Web.DOM.Element (setAttribute, toNode)
import Web.DOM.Document (toNonElementParentNode, createElement)
import Web.DOM.Node (appendChild)
import Web.DOM.NonElementParentNode (getElementById)
import Web.DOM.ParentNode (QuerySelector(..))
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

type State = Array Toot

data Action
  = LoadToots

mainNews :: Effect Unit
mainNews = runHalogenAff do
  mdiv <- selectElement $ QuerySelector "#newsdiv"
  case mdiv of
    Nothing -> pure unit
    Just div -> do
      void $ runUI component unit div

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just LoadToots
      }
    }

initialState :: forall input. input -> State
initialState = const []


render :: forall m. State -> H.ComponentHTML Action () m
render [] = HH.p_
  [ HH.text "Loading Feed"
  , HH.i [ HP.classes [ HH.ClassName "fa-solid", HH.ClassName "fa-spinner" ] ] []
  ]
render toots = HH.ul_ $ map renderToot toots
  where
    renderToot toot = HH.li_ [ HH.text toot.content ]


handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction LoadToots = do
  toots <- H.liftAff $ get_toots "109301761847534867"
  H.put toots


mainOld :: Effect Unit
mainOld = launchAff_ $ do
  log "🍝"
  r <- get_toots "109301761847534867"
  liftEffect $ do
    logShow r
    log $ "Found " <> show (length r) <> " toots"
    win <- window
    doc <- toDocument <$> document win
    maybeEl <- getElementById "toot_list" $ toNonElementParentNode doc
    case maybeEl of
      Nothing -> log "Could not find element with id 'toot-list'"
      Just toot_list -> do
        for_ r $ \toot -> do
          el <- createElement "li" doc
          setAttribute "innerHTML" toot.content el
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
