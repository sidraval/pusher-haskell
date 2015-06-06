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

module Network.Pusher.Channel (getChannelInfo) where

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

type Environment = (Pusher, Channel, [Info])

-- | @getChannelInfo pusher channel info@ requests information about a
-- particular channel for the given 'Pusher' instance. The result is either an
-- error message returned by the Pusher server, or a 'ChannelInfo' data
-- structure.

getChannelInfo :: Environment -> IO (Either String ChannelInfo)
getChannelInfo = runReaderT channelInfo

channelInfo :: ReaderT Environment IO (Either String ChannelInfo)
channelInfo = do
  (p, c, is) <- ask
  response <- liftIO . simpleHTTP . getRequest =<< generateUrl
  body <- liftIO $ getResponseBody response
  case (decode . B.pack $ body) of
    (Just c) -> return $ Right c
    Nothing -> return $ Left body

generateUrl :: ReaderT Environment IO String
generateUrl = do
  let timestamp = authTimestamp
  (p, c, is) <- ask
  withoutSignature <- urlWithoutSignature timestamp
  (++) (withoutSignature ++ "&auth_signature=")
    <$> signedAuthString timestamp

urlWithoutSignature :: Timestamp -> ReaderT Environment IO String
urlWithoutSignature t = do
  (p@(Pusher _ k _), c, is) <- ask
  liftIO $ ((++) (baseUrl p
      ++ "/channels/"
      ++ c
      ++ "?auth_version=1.0"
      ++ "&auth_key="
      ++  k
      ++ queryParamFromInfo is
      ++ "&auth_timestamp=")) <$> t

signedAuthString :: Timestamp -> ReaderT Environment IO String
signedAuthString t = do
  (p@(Pusher _ _ appSecret), c, is) <- ask
  signatureString <- unsignedAuthString t >>= return . B.pack
  return . showDigest $ hmacSha256 (B.pack appSecret) signatureString

unsignedAuthString :: Timestamp -> ReaderT Environment IO String
unsignedAuthString t = do
  (p@(Pusher appId appKey _), c, is) <- ask
  liftIO $ idKeyAndTimestamp appId appKey c
    <$> t
    >>= (\u -> return $ u ++ "&auth_version=1.0" ++ queryParamFromInfo is)

queryParamFromInfo :: [Info] -> String
queryParamFromInfo [] = mzero
queryParamFromInfo xs = "&info=" ++ (intercalate "," $ map show xs)

idKeyAndTimestamp :: String -> String -> Channel -> String -> String
idKeyAndTimestamp i k c t = "GET\n/apps/"
                            ++ i
                            ++ "/channels/"
                            ++ c
                            ++ "\nauth_key="
                            ++ k
                            ++ "&auth_timestamp="
                            ++ t
