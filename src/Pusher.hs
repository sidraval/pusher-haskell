{-# LANGUAGE OverloadedStrings #-}

module Pusher where

import Network.HTTP
import Control.Applicative
import Data.Digest.Pure.SHA
import Data.Time.Clock.POSIX
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Hash.MD5

type Channel = String

data Pusher = Pusher { pusherAppId :: String
                     , pusherAppKey :: String
                     , pusherAppSecret :: String }

data Event = Event { eventName :: String
                               , eventData :: String }

authTimestamp :: IO String
authTimestamp = show <$> round <$> getPOSIXTime

baseUrl :: Pusher -> String
baseUrl (Pusher appId _ _) = "http://api.pusherapp.com.com/apps/" ++ appId

triggerEvent :: Pusher -> Channel -> Event -> IO String
triggerEvent p c e = do
  url <- generateUrl p c e
  response <- simpleHTTP $ postRequestWithBody url contentType (requestBody c e)
  getResponseBody response

generateUrl :: Pusher -> Channel -> Event -> IO String
generateUrl = undefined

requestBody :: Channel -> Event -> String
requestBody = undefined

contentType :: String
contentType = "application/json"
