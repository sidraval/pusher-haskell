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
generateUrl p c e = undefined

requestBody :: Channel -> Event -> String
requestBody = undefined

unsignedAuthString :: Pusher -> IO String -> String -> IO String
unsignedAuthString (Pusher appId appKey _) t b =
  idKeyAndTimestamp appId appKey
  <$> t
  >>= withVersionAndBody b

idKeyAndTimestamp :: String -> String -> String -> String
idKeyAndTimestamp i k t = "POST\n/apps/"
                          ++ i
                          ++ "events\nauth_key="
                          ++ k
                          ++ "&auth_timestamp="
                          ++ t

withVersionAndBody :: String -> String -> IO String
withVersionAndBody md5body url =
  return $ url ++ "&auth_version=1.0&body_md5=" ++ md5body

contentType :: String
contentType = "application/json"
