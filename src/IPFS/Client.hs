{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module IPFS.Client where

import           Control.Monad (forM)

import           Control.Lens
import           Control.Monad.Trans.Either (EitherT)
import qualified Data.Aeson as Aeson
import           Data.Aeson hiding (Object)
import           Data.ByteString.Builder (toLazyByteString)
import           Data.ByteString.Lazy (ByteString)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Hashable (Hashable)
import           Data.Proxy (Proxy(..))
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8Builder)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Servant.API
import           Servant.Client

newtype Multiaddr = Multiaddr { _multiaddr :: Text }
                  deriving (Eq, Show, Hashable)

makeLenses ''Multiaddr

instance FromJSON Multiaddr where
  parseJSON = withText "multiaddr" (pure . Multiaddr)

instance FromJSON (Vector Multiaddr) where
  -- Sometimes a list of multiaddrs comes wrapped in an object's "Strings"
  -- key, e.g. for swarm/peers.
  parseJSON (Aeson.Object o) = do
    v <- o .: "Strings"
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

instance FromJSON (HashMap PeerID (Vector Multiaddr)) where
  parseJSON = withObject "address reply" $ \o -> do
    peers <- o .: "Addrs"
    peers & withObject "peer-address map" (\peerObj ->
      HM.fromList <$> forM (HM.toList peerObj) (\(pid, ma) ->
                              do ma' <- parseJSON ma
                                 pure (PeerID pid, ma')))

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
                  , _blockSize :: !Int
                  , _linksSize :: !Int
                  , _dataSize :: !Int
                  , _cumulativeSize :: !Int
                  } deriving (Eq, Show)

makeLenses ''ObjectStat

instance FromJSON ObjectStat where
  parseJSON = withObject "objectstat" $ \o -> do
    _objectHash <- o .: "Hash"
    _numLinks <- o .: "NumLinks"
    _blockSize <- o .: "BlockSize"
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

data Version = Version
               { _version :: !Text
               , _commit :: !Text
               } deriving (Eq, Show)

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

makeLenses ''Version

instance FromJSON Version where
  parseJSON = withObject "version" $ \o -> do
    _version <- o .: "Version"
    _commit <- o .: "Commit"
    pure Version{..}

-- Raw blocks are sent as binary but with a "text/plain" content type.
-- The OctetStream encoding instance won't accept "text/plain" and
-- there's no (MimeUnrender PlainText ByteString) instance, so we
-- create this simple encoding.
data BlockEncoding = BlockEncoding

instance Accept BlockEncoding where
  contentType _ = "text/plain"

instance MimeUnrender BlockEncoding ByteString where
  mimeUnrender _ = pure

getVersion :: EitherT ServantError IO Version
getPeers :: EitherT ServantError IO (Vector Multiaddr)
getPeerIdentity :: Maybe PeerID -> EitherT ServantError IO PeerIdentity

getLocalIdentity :: EitherT ServantError IO PeerIdentity
getLocalIdentity = getPeerIdentity Nothing

getRemoteIdentity :: PeerID -> EitherT ServantError IO PeerIdentity
getRemoteIdentity t = getPeerIdentity (Just t)

getKnownAddrs :: EitherT ServantError IO (HashMap PeerID (Vector Multiaddr))
getLocalAddrs :: EitherT ServantError IO (Vector Multiaddr)

getBlock :: Multihash -> EitherT ServantError IO ByteString

getObjectStat :: Multihash -> EitherT ServantError IO ObjectStat
getObject :: Multihash -> EitherT ServantError IO Object

type API = "api" :> "v0" :> (
       ("version" :> Get '[JSON] Version)
  :<|> ("swarm" :> (
           ("peers" :> Get '[JSON] (Vector Multiaddr))
      :<|> ("addrs" :> Get '[JSON] (HashMap PeerID (Vector Multiaddr)))
      :<|> ("addrs" :> "local" :> Get '[JSON] (Vector Multiaddr))))
  :<|> ("block" :> (
           ("get" :> Capture "blockhash" Multihash
                  :> Get '[BlockEncoding] ByteString)))
  :<|> ("object" :> (
           ("stat" :> Capture "objhash" Multihash :> Get '[JSON] ObjectStat)
      :<|> ("get" :> Capture "objhash" Multihash :> Get '[JSON] Object)))
  :<|> ("id" :> QueryParam "arg" PeerID :> Get '[JSON] PeerIdentity))

api :: Proxy API
api = Proxy

(getVersion
 :<|> (getPeers :<|> getKnownAddrs :<|> getLocalAddrs)
 :<|> getBlock
 :<|> (getObjectStat :<|> getObject)
 :<|> getPeerIdentity) =
  client api (BaseUrl Http "localhost" 5001)
