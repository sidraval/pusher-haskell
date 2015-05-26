{-# LANGUAGE OverloadedStrings #-}

module Network.Pusher.Base where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Time.Clock.POSIX

data Pusher = Pusher { pusherAppId :: String
                     , pusherAppKey :: String
                     , pusherAppSecret :: String }

data Event = Event { eventName :: String
                   , eventData :: String }

data ChannelInfo = ChannelInfo { occupied :: Bool
                               , userCount :: Maybe Int
                               , subscriptionCount :: Maybe Int
                               } deriving Show

data Info = UserCount | SubscriptionCount

instance Show Info where
  show UserCount = "user_count"
  show SubscriptionCount = "subscription_count"

instance FromJSON ChannelInfo where
  parseJSON (Object v) = ChannelInfo <$>
                         v .: "occupied" <*>
                         v .:? "user_count" <*>
                         v .:? "subscription_count"
  parseJSON _ = mzero

type Channel = String
type Channels = [Channel]
type Timestamp = IO String
type Md5Body = String

authTimestamp :: Timestamp
authTimestamp = show <$> round <$> getPOSIXTime

baseUrl :: Pusher -> String
baseUrl (Pusher appId _ _) = "http://api.pusherapp.com/apps/" ++ appId

contentType :: String
contentType = "application/json"
