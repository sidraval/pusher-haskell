{-# LANGUAGE OverloadedStrings #-}

module Pusher where

import Network.HTTP
import Control.Applicative
import Data.Digest.Pure.SHA
import Data.Time.Clock.POSIX
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Hash.MD5

data Pusher = Pusher { pusherAppId :: String
                     , pusherAppKey :: String
                     , pusherAppSecret :: String }

authTimestamp :: IO String
authTimestamp = show <$> round <$> getPOSIXTime

baseUrl :: Pusher -> String
baseUrl (Pusher appId _ _) = "http://api.pusherapp.com.com/apps/" ++ appId

triggerEvent :: Pusher -> Event -> EventData -> IO String
triggerEvent pusher event eventData = undefined
