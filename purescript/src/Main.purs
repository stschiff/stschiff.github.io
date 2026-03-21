module Main
  ( Action(..)
  , State
  , Toot
  , component
  , handleAction
  , initialState
  , mainNews
  , render
  , renderToot
  )
  where

import Prelude

import Data.Array (filter, head, concat)
import Data.DateTime (DateTime, diff)
import Data.Either (Either(..))
import Data.Formatter.DateTime (formatDateTime)
import Data.Int (floor)
import Data.JSDate (parse, toDateTime)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Days(..), Hours(..), Minutes(..), convertDuration)
import Data.Traversable (for)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Effect.Now (nowDateTime)
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

type State = {
  toots :: Array {
    toot :: Toot,
    toot_dt :: Maybe DateTime
  },
  now_dt :: Maybe DateTime
}

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
initialState = const { toots: [], now_dt: Nothing }


render :: forall m. State -> H.ComponentHTML Action () m
render state =
  case state.toots of
    [] -> HH.text "Loading toots..."
    toots -> HH.div_ $ map (\t -> renderToot state.now_dt t.toot_dt t.toot) toots

renderToot :: forall m . Maybe DateTime -> Maybe DateTime -> Toot -> H.ComponentHTML Action () m
renderToot now_dt toot_dt toot =
  HH.article [ HP.classes [ HH.ClassName "media" ] ]
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
        (concat [ [ HH.div [ HP.classes [ HH.ClassName "column", HH.ClassName "is-two-thirds"] ]
          [ HH.p_
            [ HH.a [ HP.href (toot.account.url) ]
              [ HH.strong_ [ HH.text (toot.account.display_name) ]
              , HH.text " "
              , HH.small_ [ HH.text ("@" <> toot.account.username <> "@ecoevo.social") ]
              , HH.text " "
              ]
            , time_entry
            , HH.br_
            ]
          , RH.render_ toot.content
          ]
        ], toot_img ])
      ]
    ]
  ]
 where
  toot_img = case head toot.media_attachments of
    Nothing -> []
    Just attachment -> [ HH.div [ HP.classes [ HH.ClassName "column" ] ]
        [ HH.img [ HP.src (attachment.preview_url) ] ]
      ]
  time_entry = case [now_dt, toot_dt] of
    [Just now_time, Just toot_time] ->
      let dur = diff now_time toot_time
          t =
            if dur < Hours 1.0 then
                let Minutes mins = convertDuration dur in show (floor mins) <> "m ago"
            else if convertDuration dur < Days 1.0 then
                let Hours hrs = convertDuration dur in show (floor hrs) <> "h ago"
            else if convertDuration dur < Days 7.0 then
                let Days days = convertDuration dur in show (floor days) <> "d ago"
            else
                case formatDateTime "D MMMM YYYY" toot_time of
                  Left _ -> "Invalid date"
                  Right formatted -> formatted
      in HH.small_ [ HH.text t]
    _ -> HH.small_ [ HH.text "Unknown time" ]



handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction LoadToots = do
  toots_json <- H.liftAff $ get_toots "109301761847534867"
  toots <- for toots_json $ \toot -> do
    toot_dt <- toDateTime <$> (H.liftEffect (parse toot.created_at))
    pure { toot, toot_dt }
  now_dt <- H.liftEffect $ nowDateTime
  H.put { toots, now_dt: Just now_dt }


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
