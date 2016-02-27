{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.IPFS.Client where

import           Control.Applicative ((<|>))

import           Control.Error (fmapL)
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import qualified Data.Aeson as Aeson
import           Data.Aeson hiding (Object)
import           Data.ByteString.Builder (toLazyByteString)
import           Data.ByteString.Lazy (ByteString, toStrict)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Hashable (Hashable)
import           Data.Proxy (Proxy(..))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8', encodeUtf8Builder)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Servant.API
import           Servant.Client hiding (Client)
import           Servant.Common.Req (Req, defReq, appendToPath)

type IPFSError = ServantError

newtype Multiaddr = Multiaddr { _multiaddr :: Text }
                  deriving (Eq, Show, Hashable)

makeLenses ''Multiaddr

instance FromJSON Multiaddr where
  parseJSON = withText "multiaddr" (pure . Multiaddr)

instance ToText Multiaddr where
  toText (Multiaddr ma) = ma

instance FromJSON (Vector Multiaddr) where
  -- Sometimes a list of multiaddrs comes wrapped in an object's "Strings"
  -- or "Peers" key, e.g. for swarm/peers.
  parseJSON (Aeson.Object o) = do
    v <- o .: "Peers" <|> o .: "Strings"
    V.mapM parseJSON v

  parseJSON (Array a) = V.mapM parseJSON a

  parseJSON _ = fail "expected an object or array"

newtype Multihash = Multihash { _multihash :: Text }
                  deriving (Eq, Show, Hashable)

makeLenses ''Multihash

instance FromJSON Multihash where
  parseJSON = withText "multihash" (pure . Multihash)

instance ToText Multihash where
  toText (Multihash t) = t

newtype PeerID = PeerID { _peerHash :: Text }
               deriving (Eq, Show, Hashable)

makeLenses ''PeerID

instance FromJSON PeerID where
  parseJSON = withText "peerID" (pure . PeerID)

instance ToText PeerID where
  toText (PeerID t) = t

kvApM :: Monad m => ((a -> b), (c -> m d)) -> (a, c) -> m (b, d)
kvApM (f1, f2) (a, c) = do
  let b = f1 a
  d <- f2 c
  pure (b, d)

instance FromJSON (HashMap PeerID (Vector Multiaddr)) where
  parseJSON = withObject "address reply" $ \o -> do
    peers <- o .: "Addrs"
    peers & withObject "peer-address map" (\peerObj ->
      HM.fromList <$> forM (HM.toList peerObj) (kvApM (PeerID, parseJSON)))

data PeerIdentity = PeerIdentity
                { _peerID :: !PeerID
                , _publicKey :: !Text
                , _agentVersion :: !Text
                , _protocolVersion :: !Text
                } deriving (Eq, Show)

makeLenses ''PeerIdentity

instance FromJSON PeerIdentity where
  parseJSON = withObject "identity" $ \o -> do
    _peerID <- o .: "ID"
    _publicKey <- o .: "PublicKey"
    _agentVersion <- o .: "AgentVersion"
    _protocolVersion <- o .: "ProtocolVersion"
    pure PeerIdentity{..}

data ObjectStat = ObjectStat
                  { _objectHash :: !Multihash
                  , _numLinks :: !Int
                  , _objectBlockSize :: !Int
                  , _linksSize :: !Int
                  , _dataSize :: !Int
                  , _cumulativeSize :: !Int
                  } deriving (Eq, Show)

makeLenses ''ObjectStat

instance FromJSON ObjectStat where
  parseJSON = withObject "objectstat" $ \o -> do
    _objectHash <- o .: "Hash"
    _numLinks <- o .: "NumLinks"
    _objectBlockSize <- o .: "BlockSize"
    _linksSize <- o .: "LinksSize"
    _dataSize <- o .: "DataSize"
    _cumulativeSize <- o .: "CumulativeSize"
    pure ObjectStat{..}

data ObjectLink = ObjectLink
                  { _linkedName :: !Text
                  , _linkedHash :: !Multihash
                  , _linkedSize :: !Int
                  } deriving (Eq, Show)

makeLenses ''ObjectLink

instance FromJSON ObjectLink where
  parseJSON = withObject "objectlink" $ \o -> do
    _linkedName <- o .: "Name"
    _linkedHash <- o .: "Hash"
    _linkedSize <- o .: "Size"
    pure ObjectLink{..}

instance FromJSON (Vector ObjectLink) where
  -- Support pulling from an object with a "Links" key, as returned
  -- by object/links.
  -- Is this appropriate? Should we break the objects/links response
  -- handling into a newtype to make this behaviour more transparent?
  parseJSON (Aeson.Object o) = do
    links <- o .: "Links"
    parseJSON links

  parseJSON (Array a) = V.mapM parseJSON a

  parseJSON _ = fail "expected object or array"

data Version = Version
               { _version :: !Text
               , _commit :: !(Maybe Text)
               } deriving (Eq, Show)

makeLenses ''Version

instance FromJSON Version where
  parseJSON = withObject "version" $ \o -> do
    _version <- o .: "Version"
    commitText <- o .:? "Commit"
    let _commit = case commitText of
          Nothing -> Nothing
          Just "" -> Nothing
          _ -> commitText
    pure Version{..}

data Object = Object
              { _objectData :: !ByteString
              , _objectLinks :: !(Vector ObjectLink)
              } deriving (Eq, Show)

makeLenses ''Object

instance FromJSON Object where
  parseJSON = withObject "object" $ \o -> do
    _objectData <- toLazyByteString . encodeUtf8Builder <$> o .: "Data"
    _objectLinks <- o .: "Links"
    pure Object{..}

data PinType = Direct
             | Indirect
             | Recursive
             | All
             deriving (Eq, Show)

instance FromJSON PinType where
  parseJSON (Aeson.Object o) = do
    pt <- o .: "Type"
    parseJSON pt

  parseJSON (String s)
    | s == "direct" = pure Direct
    | s == "indirect" = pure Indirect
    | s == "recursive" = pure Recursive
    | s == "all" = pure All
    | otherwise = fail "unknown pin type"

  parseJSON _ = fail "expected string or object"

instance ToText PinType where
  toText Direct = "direct"
  toText Indirect = "indirect"
  toText Recursive = "recursive"
  toText All = "all"

instance FromJSON (HashMap Multihash PinType) where
  parseJSON = withObject "pin map" $ \o -> do
    pins <- o .: "Keys"
    pins & withObject "pin map" (\pinsObj ->
      HM.fromList <$> forM (HM.toList pinsObj) (kvApM (Multihash, parseJSON)))

data BlockStat = BlockStat
                 { _blockHash :: !Multihash
                 , _blockSize :: !Int
                 } deriving (Eq, Show)

makeLenses ''BlockStat

instance FromJSON BlockStat where
  parseJSON = withObject "blockstat" $ \o -> do
    _blockHash <- o .: "Key"
    _blockSize <- o .: "Size"
    pure BlockStat{..}

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
           ("get" :> Capture "blockhash" Multihash
                  :> Get '[BlockEncoding] ByteString)
      :<|> ("stat" :> Capture "blockhash" Multihash :> Get '[JSON] BlockStat)))
  :<|> ("object" :> (
           ("stat" :> Capture "objhash" Multihash :> Get '[JSON] ObjectStat)
      :<|> ("get" :> Capture "objhash" Multihash :> Get '[JSON] Object)
      :<|> ("links" :> Capture "objhash" Multihash
                    :> Get '[JSON] (Vector ObjectLink))))
  :<|> ("pin" :> "ls" :> Get '[JSON] (HashMap Multihash PinType))
  :<|> ("refs" :> "local" :> Get '[PlainerText] (Vector Multihash))
  :<|> ("bootstrap" :> (
           (Get '[JSON] (Vector Multiaddr))
      :<|> ("add" :> QueryParam "arg" Multiaddr
                  :> Post '[JSON] (Vector Multiaddr))
      :<|> ("rm" :> QueryParam "arg" Multiaddr
                 :> Post '[JSON] (Vector Multiaddr))))
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
               , _getBlock :: Multihash -> ServantReq ByteString
               , _getBlockStat :: Multihash -> ServantReq BlockStat

               , _getObjectStat :: Multihash -> ServantReq ObjectStat
               , _getObject :: Multihash -> ServantReq Object
               , _getObjectLinks :: Multihash -> ServantReq (Vector ObjectLink)
               , _getPins :: ServantReq (HashMap Multihash PinType)
               , _getLocalRefs :: ServantReq (Vector Multihash)
               , _getBootstrapList :: ServantReq (Vector Multiaddr)
               , _addBootstrapPeer :: Maybe Multiaddr
                                   -> ServantReq (Vector Multiaddr)
               , _deleteBootstrapPeer :: Maybe Multiaddr
                                      -> ServantReq (Vector Multiaddr)
               , _getPeerIdentity :: Maybe PeerID -> ServantReq PeerIdentity
               }

mkClient :: Req -> String -> Int -> Client
mkClient req host port = Client{..}
  where (_getVersion
         :<|> (_getPeers :<|> _getKnownAddrs :<|> _getLocalAddrs)
         :<|> (_getBlock :<|> _getBlockStat)
         :<|> (_getObjectStat :<|> _getObject :<|> _getObjectLinks)
         :<|> _getPins
         :<|> _getLocalRefs
         :<|> (_getBootstrapList
          :<|> _addBootstrapPeer
          :<|> _deleteBootstrapPeer)
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

runIPFST :: MonadIO m => String -> Int -> IPFST m a -> m (Either IPFSError a)
runIPFST host port ipfs = runEitherT (runReaderT (unIPFS ipfs) cl)
  where cl = mkClient (appendToPath "api/v0" defReq) host port

runIPFS :: String -> Int -> IPFS a -> IO (Either IPFSError a)
runIPFS = runIPFST

runBareIPFST :: MonadIO m
                => String
                -> Int
                -> IPFST m a
                -> m (Either IPFSError a)
runBareIPFST host port ipfs = runEitherT (runReaderT (unIPFS ipfs) cl)
  where cl = mkClient defReq host port

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
getBlock mh = request (`_getBlock` mh)

getBlockStat :: Multihash -> IPFS BlockStat
getBlockStat mh = request (`_getBlockStat` mh)

getObjectStat :: Multihash -> IPFS ObjectStat
getObjectStat mh = request (`_getObjectStat` mh)

getObject :: Multihash -> IPFS Object
getObject mh = request (`_getObject` mh)

getObjectLinks :: Multihash -> IPFS (Vector ObjectLink)
getObjectLinks mh = request (`_getObjectLinks` mh)

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

getLocalIdentity :: IPFS PeerIdentity
getLocalIdentity = request (`_getPeerIdentity` Nothing)

getRemoteIdentity :: PeerID -> IPFS PeerIdentity
getRemoteIdentity pid = request (`_getPeerIdentity` Just pid)
