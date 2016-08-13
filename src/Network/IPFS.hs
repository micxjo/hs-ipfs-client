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
                    , runIPFS'
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
                    , getConfigValue
                    ) where

import           Control.Error (fmapL)
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Aeson as A
import           Data.ByteString.Lazy (ByteString, toStrict)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Proxy (Proxy(..))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8')
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import           Servant.API
import           Servant.Client hiding (Client)

import           Network.IPFS.Types

-- Servant's PlainText won't accept responses without a charset, which
-- go-ipfs doesn't supply.
data PlainerText

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
data BlockEncoding

instance Accept BlockEncoding where
  contentType _ = "text/plain"

instance MimeUnrender BlockEncoding ByteString where
  mimeUnrender _ = pure

-- The "Prefer" header is used to force the apiary.io mock server
-- to return a specific result.

type API = Header "Prefer" Text :> (
       ("version" :> Get '[JSON] Version)
  :<|> ("swarm" :> (
           ("peers" :> Get '[JSON] PeersResponse)
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
                    :> Get '[JSON] ObjectLinksResponse)))
  :<|> ("file" :> "ls" :> QueryParam "arg" Multihash
                       :> Get '[JSON] (HashMap Multihash FileStat))
  :<|> ("pin" :> "ls" :> Get '[JSON] (HashMap Multihash PinType))
  :<|> ("refs" :> "local" :> Get '[PlainerText] (Vector Multihash))
  :<|> ("bootstrap" :> (
           ("list" :> Get '[JSON] PeersResponse)
      :<|> ("add" :> QueryParam "arg" Multiaddr
                  :> Post '[JSON] (Vector Multiaddr))
      :<|> ("rm" :> QueryParam "arg" Multiaddr
                 :> Post '[JSON] (Vector Multiaddr))))
  :<|> ("stats" :> "bw" :> Get '[JSON] BandwidthStats)
  :<|> ("name" :> "resolve" :> QueryParam "arg" Text :> Get '[JSON] Path)
  :<|> ("id" :> QueryParam "arg" PeerID :> Get '[JSON] PeerIdentity)
  :<|> ("config" :> QueryParam "arg" Text :> Post '[JSON] ConfigResponse))

api :: Proxy API
api = Proxy

_getPeerIdentity :: Maybe PeerID -> Manager -> BaseUrl -> ClientM PeerIdentity
_resolveName :: Maybe Text -> Manager -> BaseUrl -> ClientM Path
_getBandwidthStats :: Manager -> BaseUrl -> ClientM BandwidthStats
_deleteBootstrapPeer :: Maybe Multiaddr -> Manager -> BaseUrl -> ClientM (Vector Multiaddr)
_addBootstrapPeer :: Maybe Multiaddr -> Manager -> BaseUrl -> ClientM (Vector Multiaddr)
_getBootstrapList :: Manager -> BaseUrl -> ClientM PeersResponse
_getLocalRefs :: Manager -> BaseUrl -> ClientM (Vector Multihash)
_getPins :: Manager -> BaseUrl -> ClientM (HashMap Multihash PinType)
_getFileList :: Maybe Multihash -> Manager -> BaseUrl -> ClientM (HashMap Multihash FileStat)
_getObjectLinks :: Multihash -> Manager -> BaseUrl -> ClientM ObjectLinksResponse
_getObject :: Multihash -> Manager -> BaseUrl -> ClientM Object
_getObjectStat :: Multihash -> Manager -> BaseUrl -> ClientM ObjectStat
_getBlockStat :: Maybe Multihash -> Manager -> BaseUrl -> ClientM BlockStat
_getBlock :: Maybe Multihash -> Manager -> BaseUrl -> ClientM ByteString
_getLocalAddrs :: Manager -> BaseUrl -> ClientM (Vector Multiaddr)
_getKnownAddrs :: Manager -> BaseUrl -> ClientM (HashMap PeerID (Vector Multiaddr))
_getPeers :: Manager -> BaseUrl -> ClientM PeersResponse
_getVersion :: Manager -> BaseUrl -> ClientM Version
_getConfigValue :: Maybe Text -> Manager -> BaseUrl -> ClientM ConfigResponse

(_getVersion
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
  :<|> _getConfigValue
  ) = client api (Just "status=200")

newtype IPFST m a = IPFST { unIPFS :: ReaderT (Manager, BaseUrl) (ExceptT ServantError m) a}
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader (Manager, BaseUrl)
           , MonadError ServantError
           )

instance MonadTrans IPFST where
  lift = IPFST . lift . lift

type IPFS a = IPFST IO a

runIPFST' :: MonadIO m => BaseUrl -> IPFST m a -> m (Either IPFSError a)
runIPFST' url ipfs = do
  manager <- liftIO $ newManager defaultManagerSettings
  runExceptT (runReaderT (unIPFS ipfs) (manager, url))

runIPFST :: MonadIO m => String -> Int -> IPFST m a -> m (Either IPFSError a)
runIPFST host port =
  runIPFST' (BaseUrl Http host port "/api/v0")

runIPFS' :: BaseUrl -> IPFS a -> IO (Either IPFSError a)
runIPFS' = runIPFST'

runIPFS :: String -> Int -> IPFS a -> IO (Either IPFSError a)
runIPFS = runIPFST

request :: Monad m => (Manager -> BaseUrl -> ExceptT ServantError m a) -> IPFST m a
request cmd = do
  (manager, url) <- ask
  resp <- lift (runExceptT (cmd manager url))
  case resp of
    Left l -> throwError l
    Right r -> pure r

getVersion :: IPFS Version
getVersion = request _getVersion

getPeers :: IPFS (Vector Multiaddr)
getPeers = unPeersResponse <$> request _getPeers

getKnownAddrs :: IPFS (HashMap PeerID (Vector Multiaddr))
getKnownAddrs = request _getKnownAddrs

getLocalAddrs :: IPFS (Vector Multiaddr)
getLocalAddrs = request _getLocalAddrs

getBlock :: Multihash -> IPFS ByteString
getBlock mh = request (_getBlock (Just mh))

getBlockStat :: Multihash -> IPFS BlockStat
getBlockStat mh = request (_getBlockStat (Just mh))

getObjectStat :: Multihash -> IPFS ObjectStat
getObjectStat mh = request (_getObjectStat mh)

getObject :: Multihash -> IPFS Object
getObject mh = request (_getObject mh)

getObjectLinks :: Multihash -> IPFS (Vector ObjectLink)
getObjectLinks mh = unObjectLinksResponse <$> request (_getObjectLinks mh)

getFileList :: Multihash -> IPFS FileStat
getFileList mh = do
  resp <- request (_getFileList (Just mh))
  pure (snd (head (HM.toList resp)))

getPins :: IPFS (HashMap Multihash PinType)
getPins = request _getPins

getLocalRefs :: IPFS (Vector Multihash)
getLocalRefs = request _getLocalRefs

getBootstrapList :: IPFS (Vector Multiaddr)
getBootstrapList = unPeersResponse <$> request _getBootstrapList

addBootstrapPeer :: Multiaddr -> IPFS ()
addBootstrapPeer ma = request (_addBootstrapPeer (Just ma)) >> pure ()

deleteBootstrapPeer :: Multiaddr -> IPFS ()
deleteBootstrapPeer ma = request (_deleteBootstrapPeer (Just ma)) >> pure ()

getBandwidthStats :: IPFS BandwidthStats
getBandwidthStats = request _getBandwidthStats

resolveName :: Text -> IPFS (Maybe Text)
resolveName name = do
  (manager, url) <- ask
  resp <- lift (runExceptT (_resolveName (Just name) manager url))
  case resp of
    Left _ -> pure Nothing
    Right (Path r) -> pure (Just r)

getLocalIdentity :: IPFS PeerIdentity
getLocalIdentity = request (_getPeerIdentity Nothing)

getRemoteIdentity :: PeerID -> IPFS PeerIdentity
getRemoteIdentity pid = request (_getPeerIdentity (Just pid))

getConfigValue :: Text -> IPFS A.Value
getConfigValue key = _configValue <$> request (_getConfigValue (Just key))
