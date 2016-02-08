{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module IPFS.Client where

import           Control.Monad (forM)

import           Control.Lens
import           Control.Monad.Trans.Either (EitherT)
import           Data.Aeson
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Hashable (Hashable)
import           Data.Proxy (Proxy(..))
import           Data.Text (Text)
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
  parseJSON (Object o) = do
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

data Version = Version
               { _version :: !Text
               , _commit :: !Text
               } deriving (Eq, Show)

makeLenses ''Version

instance FromJSON Version where
  parseJSON = withObject "version" $ \o -> do
    _version <- o .: "Version"
    _commit <- o .: "Commit"
    pure Version{..}

getVersion :: EitherT ServantError IO Version
getPeers :: EitherT ServantError IO (Vector Multiaddr)
getPeerIdentity :: Maybe PeerID -> EitherT ServantError IO PeerIdentity

getLocalIdentity :: EitherT ServantError IO PeerIdentity
getLocalIdentity = getPeerIdentity Nothing

getRemoteIdentity :: PeerID -> EitherT ServantError IO PeerIdentity
getRemoteIdentity t = getPeerIdentity (Just t)

getKnownAddrs :: EitherT ServantError IO (HashMap PeerID (Vector Multiaddr))
getLocalAddrs :: EitherT ServantError IO (Vector Multiaddr)

getObjectStat :: Multihash -> EitherT ServantError IO ObjectStat

type API = "api" :> "v0" :> (
       ("version" :> Get '[JSON] Version)
  :<|> ("swarm" :> (
           ("peers" :> Get '[JSON] (Vector Multiaddr))
      :<|> ("addrs" :> Get '[JSON] (HashMap PeerID (Vector Multiaddr)))
      :<|> ("addrs" :> "local" :> Get '[JSON] (Vector Multiaddr))))
  :<|> ("object" :> (
           ("stat" :> Capture "objhash" Multihash :> Get '[JSON] ObjectStat)))
  :<|> ("id" :> QueryParam "arg" PeerID :> Get '[JSON] PeerIdentity))

api :: Proxy API
api = Proxy

(getVersion
 :<|> (getPeers :<|> getKnownAddrs :<|> getLocalAddrs)
 :<|> getObjectStat
 :<|> getPeerIdentity) =
  client api (BaseUrl Http "localhost" 5001)
