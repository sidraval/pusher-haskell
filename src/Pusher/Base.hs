module Pusher.Base where

import Control.Applicative
import Data.Time.Clock.POSIX

data Pusher = Pusher { pusherAppId :: String
                     , pusherAppKey :: String
                     , pusherAppSecret :: String }

data Event = Event { eventName :: String
                   , eventData :: String }

type Channel = String
type Timestamp = IO String
type Md5Body = String

authTimestamp :: Timestamp
authTimestamp = show <$> round <$> getPOSIXTime

baseUrl :: Pusher -> String
baseUrl (Pusher appId _ _) = "http://api.pusherapp.com/apps/" ++ appId

contentType :: String
contentType = "application/json"
