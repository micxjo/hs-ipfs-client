{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.IPFS ( module Network.IPFS.Types
                    , IPFS
                    , runIPFS
                    , runBareIPFS
                    , getVersion
                    , getPeers
                    , getLocalAddrs
                    , getKnownAddrs
                    , getBlock
                    , getBlockStat
                    , getObject
                    , getObjectStat
                    , getObjectLinks
                    , getFileList
                    , getPins
                    , getLocalRefs
                    , getBootstrapList
                    , addBootstrapPeer
                    , deleteBootstrapPeer
                    , getBandwidthStats
                    , resolveName
                    , getLocalIdentity
                    , getRemoteIdentity
                    ) where

import           Control.Error (fmapL)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import           Data.ByteString.Lazy (ByteString, toStrict)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Proxy (Proxy(..))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8')
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Servant.API
import           Servant.Client hiding (Client)
import           Servant.Common.Req (Req)
import qualified Servant.Common.Req as Req

import           Network.IPFS.Types

-- Servant's PlainText won't accept responses without a charset, which
-- go-ipfs doesn't supply.
data PlainerText = PlainerText

instance Accept PlainerText where
  contentType _ = "text/plain"

-- Unrenderer for a newline-separated list of Multihashes
instance MimeUnrender PlainerText (Vector Multihash) where
  mimeUnrender _ bs = do
    t <- fmapL show (decodeUtf8' (toStrict bs))
    pure (V.fromList (map Multihash (T.lines t)))

-- Raw blocks are sent as binary but with a "text/plain" content type.
-- The OctetStream encoding instance won't accept "text/plain" and
-- there's no (MimeUnrender PlainText ByteString) instance, so we
-- create this simple encoding.
data BlockEncoding = BlockEncoding

instance Accept BlockEncoding where
  contentType _ = "text/plain"

instance MimeUnrender BlockEncoding ByteString where
  mimeUnrender _ = pure

type API = (
       ("version" :> Get '[JSON] Version)
  :<|> ("swarm" :> (
           ("peers" :> Get '[JSON] (Vector Multiaddr))
      :<|> ("addrs" :> Get '[JSON] (HashMap PeerID (Vector Multiaddr)))
      :<|> ("addrs" :> "local" :> Get '[JSON] (Vector Multiaddr))))
  :<|> ("block" :> (
           ("get" :> QueryParam "arg" Multihash
                  :> Get '[BlockEncoding] ByteString)
      :<|> ("stat" :> QueryParam "arg" Multihash
                   :> Get '[JSON] BlockStat)))
  :<|> ("object" :> (
           ("stat" :> Capture "objhash" Multihash :> Get '[JSON] ObjectStat)
      :<|> ("get" :> Capture "objhash" Multihash :> Get '[JSON] Object)
      :<|> ("links" :> Capture "objhash" Multihash
                    :> Get '[JSON] (Vector ObjectLink))))
  :<|> ("file" :> "ls" :> QueryParam "arg" Multihash
                       :> Get '[JSON] (HashMap Multihash FileStat))
  :<|> ("pin" :> "ls" :> Get '[JSON] (HashMap Multihash PinType))
  :<|> ("refs" :> "local" :> Get '[PlainerText] (Vector Multihash))
  :<|> ("bootstrap" :> (
           ("list" :> Get '[JSON] (Vector Multiaddr))
      :<|> ("add" :> QueryParam "arg" Multiaddr
                  :> Post '[JSON] (Vector Multiaddr))
      :<|> ("rm" :> QueryParam "arg" Multiaddr
                 :> Post '[JSON] (Vector Multiaddr))))
  :<|> ("stats" :> "bw" :> Get '[JSON] BandwidthStats)
  :<|> ("name" :> "resolve" :> QueryParam "arg" Text :> Get '[JSON] Path)
  :<|> ("id" :> QueryParam "arg" PeerID :> Get '[JSON] PeerIdentity))

api :: Proxy API
api = Proxy

type ServantReq = EitherT ServantError IO

data Client = Client
              { _getVersion :: ServantReq Version
              , _getPeers :: ServantReq (Vector Multiaddr)
              , _getKnownAddrs :: ServantReq (
                  HashMap PeerID (Vector Multiaddr))
              , _getLocalAddrs :: ServantReq (Vector Multiaddr)
              , _getBlock :: Maybe Multihash -> ServantReq ByteString
              , _getBlockStat :: Maybe Multihash -> ServantReq BlockStat
              , _getObjectStat :: Multihash -> ServantReq ObjectStat
              , _getObject :: Multihash -> ServantReq Object
              , _getObjectLinks :: Multihash -> ServantReq (Vector ObjectLink)
              , _getFileList :: Maybe Multihash
                                -> ServantReq (HashMap Multihash FileStat)
              , _getPins :: ServantReq (HashMap Multihash PinType)
              , _getLocalRefs :: ServantReq (Vector Multihash)
              , _getBootstrapList :: ServantReq (Vector Multiaddr)
              , _addBootstrapPeer :: Maybe Multiaddr
                                     -> ServantReq (Vector Multiaddr)
              , _deleteBootstrapPeer :: Maybe Multiaddr
                                        -> ServantReq (Vector Multiaddr)
              , _getBandwidthStats :: ServantReq BandwidthStats
              , _resolveName :: Maybe Text -> ServantReq Path
              , _getPeerIdentity :: Maybe PeerID -> ServantReq PeerIdentity
              }

mkClient :: Req -> String -> Int -> Client
mkClient req host port = Client{..}
  where (_getVersion
         :<|> (_getPeers :<|> _getKnownAddrs :<|> _getLocalAddrs)
         :<|> (_getBlock :<|> _getBlockStat)
         :<|> (_getObjectStat :<|> _getObject :<|> _getObjectLinks)
         :<|> _getFileList
         :<|> _getPins
         :<|> _getLocalRefs
         :<|> (_getBootstrapList
          :<|> _addBootstrapPeer
          :<|> _deleteBootstrapPeer)
         :<|> _getBandwidthStats
         :<|> _resolveName
         :<|> _getPeerIdentity
          ) = clientWithRoute api req (BaseUrl Http host port)

newtype IPFST m a = IPFS { unIPFS :: ReaderT Client (EitherT ServantError m) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader Client
           , MonadError ServantError
           )

instance MonadTrans IPFST where
  lift = IPFS . lift . lift

type IPFS a = IPFST IO a

baseReq :: Req
baseReq = Req.addHeader "Prefer" ("status=200" :: Text) Req.defReq

runIPFST :: MonadIO m => String -> Int -> IPFST m a -> m (Either IPFSError a)
runIPFST host port ipfs = runEitherT (runReaderT (unIPFS ipfs) cl)
  where cl = mkClient (Req.appendToPath "api/v0" baseReq) host port

runIPFS :: String -> Int -> IPFS a -> IO (Either IPFSError a)
runIPFS = runIPFST

runBareIPFST :: MonadIO m
                => String
                -> Int
                -> IPFST m a
                -> m (Either IPFSError a)
runBareIPFST host port ipfs = runEitherT (runReaderT (unIPFS ipfs) cl)
  where cl = mkClient baseReq host port

runBareIPFS :: String -> Int -> IPFS a -> IO (Either IPFSError a)
runBareIPFS = runBareIPFST

request :: Monad m => (Client -> EitherT ServantError m a) -> IPFST m a
request cmd = do
  c <- ask
  resp <- lift (runEitherT (cmd c))
  case resp of
    Left l -> throwError l
    Right r -> pure r

getVersion :: IPFS Version
getVersion = request _getVersion

getPeers :: IPFS (Vector Multiaddr)
getPeers = request _getPeers

getKnownAddrs :: IPFS (HashMap PeerID (Vector Multiaddr))
getKnownAddrs = request _getKnownAddrs

getLocalAddrs :: IPFS (Vector Multiaddr)
getLocalAddrs = request _getLocalAddrs

getBlock :: Multihash -> IPFS ByteString
getBlock mh = request (`_getBlock` Just mh)

getBlockStat :: Multihash -> IPFS BlockStat
getBlockStat mh = request (`_getBlockStat` Just mh)

getObjectStat :: Multihash -> IPFS ObjectStat
getObjectStat mh = request (`_getObjectStat` mh)

getObject :: Multihash -> IPFS Object
getObject mh = request (`_getObject` mh)

getObjectLinks :: Multihash -> IPFS (Vector ObjectLink)
getObjectLinks mh = request (`_getObjectLinks` mh)

getFileList :: Multihash -> IPFS FileStat
getFileList mh = do
  resp <- request (`_getFileList` Just mh)
  pure (snd (head (HM.toList resp)))

getPins :: IPFS (HashMap Multihash PinType)
getPins = request _getPins

getLocalRefs :: IPFS (Vector Multihash)
getLocalRefs = request _getLocalRefs

getBootstrapList :: IPFS (Vector Multiaddr)
getBootstrapList = request _getBootstrapList

addBootstrapPeer :: Multiaddr -> IPFS ()
addBootstrapPeer ma = request (`_addBootstrapPeer` Just ma) >> pure ()

deleteBootstrapPeer :: Multiaddr -> IPFS ()
deleteBootstrapPeer ma = request (`_deleteBootstrapPeer` Just ma) >> pure ()

getBandwidthStats :: IPFS BandwidthStats
getBandwidthStats = request _getBandwidthStats

resolveName :: Text -> IPFS (Maybe Text)
resolveName name = do
  c <- ask
  resp <- lift (runEitherT (_resolveName c (Just name)))
  case resp of
    Left _ -> pure Nothing
    Right (Path r) -> pure (Just r)

getLocalIdentity :: IPFS PeerIdentity
getLocalIdentity = request (`_getPeerIdentity` Nothing)

getRemoteIdentity :: PeerID -> IPFS PeerIdentity
getRemoteIdentity pid = request (`_getPeerIdentity` Just pid)
