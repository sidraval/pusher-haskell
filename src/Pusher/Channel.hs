{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pusher.Channel where

import Network.HTTP
import Control.Applicative
import Data.Digest.Pure.SHA
import Data.Time.Clock.POSIX
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Hash.MD5
import Pusher.Base

type Channel = String
type Timestamp = IO String

authTimestamp :: Timestamp
authTimestamp = show <$> round <$> getPOSIXTime

baseUrl :: Pusher -> String
baseUrl (Pusher appId _ _) = "http://api.pusherapp.com/apps/" ++ appId

getChannelInfo :: Pusher -> Channel -> IO String
getChannelInfo p c = do
  url <- generateUrl p c
  response <- simpleHTTP $ getRequest url
  getResponseBody response

-- Generate full URL for posting to Pusher
generateUrl :: Pusher -> Channel -> IO String
generateUrl p c = do
  let timestamp = authTimestamp
  withoutSignature <- urlWithoutSignature p c timestamp
  (++) (withoutSignature  ++ "&auth_signature=")
    <$> signedAuthString p c timestamp

urlWithoutSignature :: Pusher -> Channel -> Timestamp -> IO String
urlWithoutSignature p@(Pusher _ k _) c t = ((++) (baseUrl p
                                                  ++ "/channels/"
                                                  ++ c
                                                  ++ "?auth_version=1.0"
                                                  ++ "&auth_key="
                                                  ++ k
                                                  ++ "&auth_timestamp="))
                                            <$> t

-- Signed authentication string
signedAuthString :: Pusher -> Channel -> Timestamp -> IO String
signedAuthString p@(Pusher _ _ appSecret) c t = do
  signatureString <- unsignedAuthString p c t >>= return . B.pack
  return . showDigest $ hmacSha256 (B.pack appSecret) signatureString

-- Full string ready to be signed by the app secret
unsignedAuthString :: Pusher -> Channel -> Timestamp -> IO String
unsignedAuthString (Pusher appId appKey _) c t =
  idKeyAndTimestamp appId appKey c
  <$> t
  >>= withVersion

-- Helper for unsignedAuthString
idKeyAndTimestamp :: String -> String -> Channel -> String -> String
idKeyAndTimestamp i k c t = "GET\n/apps/"
                            ++ i
                            ++ "/channels/"
                            ++ c
                            ++ "\nauth_key="
                            ++ k
                            ++ "&auth_timestamp="
                            ++ t

-- Helper for unsignedAuthString
withVersion :: String -> IO String
withVersion url =
  return $ url ++ "&auth_version=1.0"

contentType :: String
contentType = "application/json"
