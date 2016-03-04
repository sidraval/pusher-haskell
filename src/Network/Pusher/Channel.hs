-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Pusher.Channel
-- Copyright   :  See LICENSE file
-- License     :  BSD
--
-- Maintainer  :  Sid Raval <sidsraval@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (not tested)
--
-- The 'Network.Pusher.Channel' module provides an simple interface for interacting
-- with Pusher.com's @channel@ endpoints. In particular, one can fetch info
-- about every occupied channel for a given App ID, or more detailed information
-- about a channel specified by name.
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.Pusher.Channel (getChannelInfo, getMultiChannelInfo) where

import Network.HTTP
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Data.Aeson
import Data.Digest.Pure.SHA
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Hash.MD5
import Data.List
import Network.Pusher.Base

type Environment a = (Pusher, a, [Info], Prefix)

class ChannelURL a where
  urlify :: a -> String

instance ChannelURL ChannelName where
  urlify a = "/channels/" ++ a

instance ChannelURL ChannelNames where
  urlify _ = "/channels"

-- | @getChannelInfo (pusher, channel, info)@ requests information about a
-- particular channel for the given 'Pusher' instance. The result is either an
-- error message returned by the Pusher server, or a 'ChannelInfo' data
-- structure.

getChannelInfo :: (Pusher, ChannelName, [Info]) -> IO (Either String ChannelInfo)
getChannelInfo (p, c, i) = runReaderT channelInfo (p, c, i, Nothing)

-- | @getMultiChannelInfo (pusher, info, prefix)@ requests information about all
-- channels for the given 'Pusher' instance. The result is either an
-- error message returned by the Pusher server, or a 'ChannelList' data
-- structure.

getMultiChannelInfo :: (Pusher, [Info], Prefix) -> IO (Either String ChannelList)
getMultiChannelInfo (p, i, f) = runReaderT channelInfo (p, [] :: [String], i, f)

channelInfo :: (ChannelURL a, FromJSON b) => ReaderT (Environment a) IO (Either String b)
channelInfo = do
  response <- liftIO . simpleHTTP . getRequest =<< generateUrl
  body <- liftIO $ getResponseBody response
  case (decode . B.pack $ body) of
    (Just c) -> return $ Right c
    Nothing -> return $ Left body

generateUrl :: (ChannelURL a) => ReaderT (Environment a) IO String
generateUrl = do
  let timestamp = authTimestamp
  (p, c, is, f) <- ask
  withoutSignature <- urlWithoutSignature timestamp
  (++) (withoutSignature ++ "&auth_signature=")
    <$> signedAuthString timestamp

urlWithoutSignature :: (ChannelURL a) => Timestamp -> ReaderT (Environment a) IO String
urlWithoutSignature t = do
  (p@(Pusher _ k _), c, is, f) <- ask
  liftIO $ ((++) (baseUrl p
      ++ urlify c
      ++ "?auth_version=1.0"
      ++ "&auth_key="
      ++  k
      ++ queryParamFromInfo is
      ++ queryParamForPrefix f
      ++ "&auth_timestamp=")) <$> t

signedAuthString :: (ChannelURL a) => Timestamp -> ReaderT (Environment a) IO String
signedAuthString t = do
  (p@(Pusher _ _ appSecret), c, is, f) <- ask
  signatureString <- unsignedAuthString t >>= return . B.pack
  return . showDigest $ hmacSha256 (B.pack appSecret) signatureString

unsignedAuthString :: (ChannelURL a) => Timestamp -> ReaderT (Environment a) IO String
unsignedAuthString t = do
  (p@(Pusher appId appKey _), c, is, f) <- ask
  liftIO $ idKeyAndTimestamp appId appKey c
    <$> t
    >>= (\u -> return $ u ++ "&auth_version=1.0" ++ queryParamFromInfo is ++ queryParamForPrefix f)

queryParamFromInfo :: [Info] -> String
queryParamFromInfo [] = mzero
queryParamFromInfo xs = "&info=" ++ (intercalate "," $ map show xs)

queryParamForPrefix :: Maybe String -> String
queryParamForPrefix Nothing = ""
queryParamForPrefix (Just p) = "&filter_by_prefix=" ++ p

idKeyAndTimestamp :: (ChannelURL a) => String -> String -> a -> String -> String
idKeyAndTimestamp i k c t = "GET\n/apps/"
                            ++ i
                            ++ urlify c
                            ++ "\nauth_key="
                            ++ k
                            ++ "&auth_timestamp="
                            ++ t
