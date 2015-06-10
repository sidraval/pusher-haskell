-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Pusher.Event
-- Copyright   :  See LICENSE file
-- License     :  BSD
--
-- Maintainer  :  Sid Raval <sidsraval@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (not tested)
--
-- The 'Network.Pusher.Event' module provides an simple interface for interacting
-- with Pusher.com's @event@ endpoints. This is used for trigger an event on one
-- or more channels with arbitrary data.
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.Pusher.Event (triggerEvent, triggerMultiChannelEvent) where

import Network.HTTP
import Control.Applicative
import Data.Aeson.Encode (encode)
import Data.Digest.Pure.SHA
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Hash.MD5
import Network.Pusher.Base

-- | @triggerEvent pusher channel event@ sends an event to one
-- channel for the given 'Pusher' instance. The result is the response body
-- from the Pusher server.
triggerEvent :: Pusher -> Channel -> Event -> IO String
triggerEvent p c e = do
  let b = requestBody c e
  url <- generateUrl p b e
  response <- simpleHTTP $ postRequestWithBody url contentType b
  getResponseBody response

-- | @triggerMultiChannelEvent pusher channels event@ sends an event to multiple
-- channels for the given 'Pusher' instance. The result is the response body
-- from the Pusher server.
triggerMultiChannelEvent :: Pusher -> Channels -> Event -> IO String
triggerMultiChannelEvent p cs e = do
  let b = requestMultiChannelBody cs e
  url <- generateUrl p b e
  response <- simpleHTTP $ postRequestWithBody url contentType b
  getResponseBody response

generateUrl :: Pusher -> String -> Event -> IO String
generateUrl p b e = do
  let md5body = md5s . Str $ b
  let timestamp = authTimestamp
  withoutSignature <- urlWithoutSignature p md5body timestamp
  (++) (withoutSignature  ++ "&auth_signature=")
    <$> signedAuthString p timestamp md5body

urlWithoutSignature :: Pusher -> Md5Body -> Timestamp -> IO String
urlWithoutSignature p@(Pusher _ k _) b t = ((++) (baseUrl p
                                                  ++ "/events?body_md5="
                                                  ++ b
                                                  ++ "&auth_version=1.0&auth_key="
                                                  ++ k
                                                  ++ "&auth_timestamp="))
                                            <$> t

requestBody c e = "{\"name\": \""
                ++ (eventName e)
                  ++ "\", \"channel\": \""
                  ++ c
                  ++ "\", \"data\":"
                  ++ (B.unpack . encode . eventData $ e)
                  ++ "}"

requestMultiChannelBody cs e = "{\"name\": \""
                             ++ (eventName e)
                             ++ "\", \"channels\": "
                             ++ show cs
                             ++ ", \"data\":"
                             ++ (B.unpack . encode . eventData $ e)
                             ++ "}"

signedAuthString :: Pusher -> Timestamp -> Md5Body -> IO String
signedAuthString p@(Pusher _ _ appSecret) t b = do
  signatureString <- unsignedAuthString p t b >>= return . B.pack
  return . showDigest $ hmacSha256 (B.pack appSecret) signatureString

unsignedAuthString :: Pusher -> Timestamp -> Md5Body -> IO String
unsignedAuthString (Pusher appId appKey _) t b =
  idKeyAndTimestamp appId appKey
  <$> t
  >>= (\u -> return $ u ++ "&auth_version=1.0&body_md5=" ++ b)

-- Helper for unsignedAuthString
idKeyAndTimestamp :: String -> String -> String -> String
idKeyAndTimestamp i k t = "POST\n/apps/"
                          ++ i
                          ++ "/events\nauth_key="
                          ++ k
                          ++ "&auth_timestamp="
                          ++ t
