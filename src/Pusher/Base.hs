module Pusher.Base where

data Pusher = Pusher { pusherAppId :: String
                     , pusherAppKey :: String
                     , pusherAppSecret :: String }

data Event = Event { eventName :: String
                   , eventData :: String }
