module Main where

import Prelude

import Data.Array (filter)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Fetch (fetch)
import Fetch.Argonaut.Json (fromJson)
import Halogen as H
import Halogen.Aff (runHalogenAff)
import Halogen.Aff.Util (selectElement)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Html.Renderer.Halogen as RH
import Web.DOM.ParentNode (QuerySelector(..))

type Toot = {
  in_reply_to_id :: Maybe String,
  media_attachments :: Array ({
    url :: String,
    preview_url :: String
  }),
  created_at :: String,
  account :: {
    url :: String,
    avatar :: String,
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
render toots = HH.div_ $ map renderToot toots
  where
    renderToot toot = HH.article [ HP.classes [ HH.ClassName "media" ] ]
      [ HH.figure [ HP.classes [ HH.ClassName "media-left" ] ]
        [ HH.a
          [ HP.href (toot.url)
          , HP.classes [ HH.ClassName "image", HH.ClassName "is-64x64" ]
          ]
          [ HH.img [ HP.src (toot.account.avatar) ] ]
        ]
        , HH.div [ HP.classes [ HH.ClassName "media-content"] ]
          [ HH.div [ HP.classes [ HH.ClassName "content"] ]
            [ HH.div [ HP.classes [ HH.ClassName "columns"] ]
              [ HH.div [ HP.classes [ HH.ClassName "column", HH.ClassName "is-two-thirds"] ]
                [ HH.p_
                  [ HH.a [ HP.href (toot.account.url) ]
                    [ HH.strong_ [ HH.text (toot.account.display_name) ]
                    , HH.text " "
                    , HH.small_ [ HH.text ("@" <> toot.account.username <> "@ecoevo.social") ]
                    , HH.text " "
                    ]
                  , HH.small_ [ HH.text "this should be the time" ]
                  , HH.br_
                  ]
                , RH.render_ toot.content
                ]
              ]
            ]
          ]
      ]


handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction LoadToots = do
  toots <- H.liftAff $ get_toots "109301761847534867"
  H.put toots


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
