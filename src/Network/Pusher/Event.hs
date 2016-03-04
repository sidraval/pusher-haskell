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
import Control.Monad
import Control.Monad.Reader
import Data.Aeson.Encode (encode)
import Data.Digest.Pure.SHA
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Hash.MD5
import Network.Pusher.Base

type Environment = (Pusher, String, Event)

-- | @triggerEvent (pusher, channelName, event)@ sends an event to one
-- channel for the given 'Pusher' instance. The result is the response body
-- from the Pusher server.
triggerEvent :: (Pusher, ChannelName, Event) -> IO String
triggerEvent (p, c, e) = runReaderT event (p, requestBody c e, e)

-- | @triggerMultiChannelEvent (pusher, channelNames, event)@ sends an event to multiple
-- channels for the given 'Pusher' instance. The result is the response body
-- from the Pusher server.
triggerMultiChannelEvent :: (Pusher, ChannelNames, Event) -> IO String
triggerMultiChannelEvent (p, cs, e) = runReaderT event (p, requestMultiChannelBody cs e, e)

event :: ReaderT Environment IO String
event = do
  (p, b, e) <- ask
  url <- generateUrl
  response <- liftIO . simpleHTTP $ postRequestWithBody url contentType b
  liftIO $ getResponseBody response

generateUrl :: ReaderT Environment IO String
generateUrl = do
  (p, b, e) <- ask
  let md5body = md5s . Str $ b
  let timestamp = authTimestamp
  withoutSignature <- urlWithoutSignature md5body timestamp
  (++) (withoutSignature  ++ "&auth_signature=")
    <$> signedAuthString timestamp md5body

urlWithoutSignature :: Md5Body -> Timestamp -> ReaderT Environment IO String
urlWithoutSignature b t = do
  (p@(Pusher _ k _), _, _) <- ask
  liftIO $ ((++) (baseUrl p
             ++ "/events?body_md5="
             ++ b
             ++ "&auth_version=1.0&auth_key="
             ++ k
             ++ "&auth_timestamp=")) <$> t

signedAuthString :: Timestamp -> Md5Body -> ReaderT Environment IO String
signedAuthString t b = do
  (p@(Pusher _ _ appSecret), _, _) <- ask
  signatureString <- unsignedAuthString t b >>= return . B.pack
  return . showDigest $ hmacSha256 (B.pack appSecret) signatureString

unsignedAuthString :: Timestamp -> Md5Body -> ReaderT Environment IO String
unsignedAuthString t b = do
  (p@(Pusher appId appKey _), _, _) <- ask
  liftIO $ idKeyAndTimestamp appId appKey
    <$> t
    >>= (\u -> return $ u ++ "&auth_version=1.0&body_md5=" ++ b)

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

-- Helper for unsignedAuthString
idKeyAndTimestamp :: String -> String -> String -> String
idKeyAndTimestamp i k t = "POST\n/apps/"
                          ++ i
                          ++ "/events\nauth_key="
                          ++ k
                          ++ "&auth_timestamp="
                          ++ t
