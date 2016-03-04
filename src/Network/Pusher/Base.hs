{-# LANGUAGE OverloadedStrings #-}

module Network.Pusher.Base where

import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.Map as M
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


data PartialChannel = PartialChannel { pUserCount :: Maybe Int } deriving Show

instance FromJSON PartialChannel where
  parseJSON (Object v) = PartialChannel <$> (v .:? "user_count")
  parseJSON _ = mzero

data Channel = Channel { name :: String, cUserCount :: Maybe Int } deriving Show

newtype ChannelList = ChannelList [Channel] deriving Show

instance FromJSON ChannelList where
  parseJSON (Object v) = (ChannelList . map toChannel . M.toList) <$> (v .: "channels")

toChannel :: (String, PartialChannel) -> Channel
toChannel (n, PartialChannel u) = Channel n u

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

type Prefix = Maybe String
type ChannelName = String
type ChannelNames = [ChannelName]
type Timestamp = IO String
type Md5Body = String

authTimestamp :: Timestamp
authTimestamp = show <$> round <$> getPOSIXTime

baseUrl :: Pusher -> String
baseUrl (Pusher appId _ _) = "http://api.pusherapp.com/apps/" ++ appId

contentType :: String
contentType = "application/json"
