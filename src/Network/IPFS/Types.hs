{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Network.IPFS.Types where

import           Control.Applicative ((<|>))
import           Control.Monad (forM)
import           Data.Data (Data)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)

import           Control.Lens
import qualified Data.Aeson as A
import           Data.Aeson hiding (Object)
import           Data.ByteString.Builder (toLazyByteString)
import           Data.ByteString.Lazy (ByteString)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Hashable (Hashable)
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8Builder)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Servant.Client (ServantError)
import           Web.HttpApiData (ToHttpApiData(..))

type IPFSError = ServantError

newtype Multiaddr = Multiaddr { unMultiaddr :: Text }
                  deriving (Eq, Show, Read, Hashable, Typeable, Data, Generic)

instance FromJSON Multiaddr where
  parseJSON = withText "multiaddr" (pure . Multiaddr)

instance ToHttpApiData Multiaddr where
  toUrlPiece (Multiaddr ma) = ma

newtype PeersResponse = PeersResponse { unPeersResponse :: Vector Multiaddr }
                 deriving (Show)

instance FromJSON PeersResponse where
  parseJSON (A.Object o) = do
    v <- o .: "Peers" <|> o .: "Strings"
    PeersResponse <$> V.mapM parseJSON v

  parseJSON _ = fail "expected an object with 'Peers' key"

newtype Multihash = Multihash { unMultihash :: Text }
                  deriving (Eq, Show, Read, Hashable, Typeable, Data, Generic)

instance FromJSON Multihash where
  parseJSON = withText "multihash" (pure . Multihash)

instance ToHttpApiData Multihash where
  toUrlPiece (Multihash t) = t

newtype PeerID = PeerID { unPeerID :: Text }
               deriving (Eq, Show, Read, Hashable, Typeable, Data, Generic)

instance FromJSON PeerID where
  parseJSON = withText "peerID" (pure . PeerID)

instance ToHttpApiData PeerID where
  toUrlPiece (PeerID t) = t

newtype Path = Path Text
             deriving (Eq, Show, Read, Hashable, Typeable, Data, Generic)

instance FromJSON Path where
  parseJSON = withObject "path obj" $ \o ->
    Path <$> o .: "Path"

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

data ObjectStat = ObjectStat
                  { _oStatHash :: !Multihash
                  , _oStatNumLinks :: !Int
                  , _oStatBlockSize :: !Int
                  , _oStatLinksSize :: !Int
                  , _oStatDataSize :: !Int
                  , _oStatCumulativeSize :: !Int
                  } deriving (Eq, Show, Read, Typeable, Data, Generic)

instance Hashable ObjectStat where

makeLenses ''ObjectStat

instance FromJSON ObjectStat where
  parseJSON = withObject "objectstat" $ \o -> do
    _oStatHash <- o .: "Hash"
    _oStatNumLinks <- o .: "NumLinks"
    _oStatBlockSize <- o .: "BlockSize"
    _oStatLinksSize <- o .: "LinksSize"
    _oStatDataSize <- o .: "DataSize"
    _oStatCumulativeSize <- o .: "CumulativeSize"
    pure ObjectStat{..}

data BlockStat = BlockStat
                 { _bStatHash :: !Multihash
                 , _bStatSize :: !Int
                 } deriving (Eq, Show, Read, Typeable, Data, Generic)

instance Hashable BlockStat where

makeLenses ''BlockStat

instance FromJSON BlockStat where
  parseJSON = withObject "blockstat" $ \o -> do
    _bStatHash <- o .: "Key"
    _bStatSize <- o .: "Size"
    pure BlockStat{..}

data PeerIdentity = PeerIdentity
                { _pidID :: !PeerID
                , _pidPublicKey :: !Text
                , _pidAgentVersion :: !Text
                , _pidProtocolVersion :: !Text
                } deriving (Eq, Show, Read, Typeable, Data, Generic)

instance Hashable PeerIdentity where

makeLenses ''PeerIdentity

instance FromJSON PeerIdentity where
  parseJSON = withObject "identity" $ \o -> do
    _pidID <- o .: "ID"
    _pidPublicKey <- o .: "PublicKey"
    _pidAgentVersion <- o .: "AgentVersion"
    _pidProtocolVersion <- o .: "ProtocolVersion"
    pure PeerIdentity{..}

data ObjectLink = ObjectLink
                  { _oLinkName :: !Text
                  , _oLinkHash :: !Multihash
                  , _oLinkSize :: !Int
                  } deriving (Eq, Show, Read, Typeable, Data, Generic)

instance Hashable ObjectLink where

makeLenses ''ObjectLink

instance FromJSON ObjectLink where
  parseJSON = withObject "objectlink" $ \o -> do
    _oLinkName <- o .: "Name"
    _oLinkHash <- o .: "Hash"
    _oLinkSize <- o .: "Size"
    pure ObjectLink{..}

newtype ObjectLinksResponse =
  ObjectLinksResponse { unObjectLinksResponse :: Vector ObjectLink
                      } deriving (Show)

instance FromJSON ObjectLinksResponse where
  parseJSON (A.Object o) = ObjectLinksResponse <$> o .: "Links"

  parseJSON _ = fail "expected an object with 'Links' key"

data Version = Version
               { _versionText :: !Text
               , _versionCommit :: !(Maybe Text)
               } deriving (Eq, Show, Read, Typeable, Data, Generic)

instance Hashable Version where

makeLenses ''Version

instance FromJSON Version where
  parseJSON = withObject "version" $ \o -> do
    _versionText <- o .: "Version"
    commitText <- o .:? "Commit"
    let _versionCommit = case commitText of
          Nothing -> Nothing
          Just "" -> Nothing
          _ -> commitText
    pure Version{..}

data Object = Object
              { _oData :: !ByteString
              , _oLinks :: !(Vector ObjectLink)
              } deriving (Eq, Show, Read, Typeable, Data, Generic)

makeLenses ''Object

instance FromJSON Object where
  parseJSON = withObject "object" $ \o -> do
    _oData <- toLazyByteString . encodeUtf8Builder <$> o .: "Data"
    _oLinks <- o .: "Links"
    pure Object{..}

data PinType = Direct
             | Indirect
             | Recursive
             | All
             deriving (Eq, Show, Read, Typeable, Data, Generic)

instance Hashable PinType where

makePrisms ''PinType

instance FromJSON PinType where
  parseJSON (A.Object o) = do
    pt <- o .: "Type"
    parseJSON pt

  parseJSON (String s)
    | s == "direct" = pure Direct
    | s == "indirect" = pure Indirect
    | s == "recursive" = pure Recursive
    | s == "all" = pure All
    | otherwise = fail "unknown pin type"

  parseJSON _ = fail "expected string or object"

instance ToHttpApiData PinType where
  toUrlPiece Direct = "direct"
  toUrlPiece Indirect = "indirect"
  toUrlPiece Recursive = "recursive"
  toUrlPiece All = "all"

instance FromJSON (HashMap Multihash PinType) where
  parseJSON = withObject "pin map" $ \o -> do
    pins <- o .: "Keys"
    pins & withObject "pin map" (\pinsObj ->
      HM.fromList <$> forM (HM.toList pinsObj) (kvApM (Multihash, parseJSON)))

data FileType = File
              | Directory
              deriving (Eq, Show, Read, Typeable, Data, Generic)

instance Hashable FileType where

makePrisms ''FileType

instance FromJSON FileType where
  parseJSON = withText "file type" $ \s ->
    case s of
      "File" -> pure File
      "Directory" -> pure Directory
      _ -> fail "invalid file type"

data FileLink = FileLink { _fLinkName :: !Text
                         , _fLinkHash :: !Multihash
                         , _fLinkSize :: !Int
                         , _fLinkType :: !FileType
                         } deriving (Eq, Show, Read, Typeable, Data, Generic)

instance Hashable FileLink where

makeLenses ''FileLink

instance FromJSON FileLink where
  parseJSON = withObject "file link" $ \o -> do
    _fLinkName <- o .: "Name"
    _fLinkHash <- o .: "Hash"
    _fLinkSize <- o .: "Size"
    _fLinkType <- o .: "Type"
    pure FileLink{..}

data FileStat = FileStat { _fStatHash :: !Multihash
                         , _fStatSize :: !Int
                         , _fStatType :: !FileType
                         , _fStatLinks :: !(Maybe (Vector FileLink))
                         } deriving (Eq, Show, Read, Typeable, Data, Generic)

makeLenses ''FileStat

instance FromJSON FileStat where
  parseJSON = withObject "file stat" $ \o -> do
    _fStatHash <- o .: "Hash"
    _fStatSize <- o .: "Size"
    _fStatType <- o .: "Type"
    _fStatLinks <- o .: "Links"
    pure FileStat{..}

instance FromJSON (HashMap Multihash FileStat) where
  parseJSON = withObject "ls reply" $ \o -> do
    objs <- o .: "Objects"
    objs & withObject "objs" (\obj ->
      HM.fromList <$> forM (HM.toList obj) (kvApM (Multihash, parseJSON)))

data BandwidthStats = BandwidthStats { _bwTotalIn :: !Integer
                                     , _bwTotalOut :: !Integer
                                     , _bwRateIn :: !Integer
                                     , _bwRateOut :: !Integer
                                     }
                    deriving (Eq, Show, Read, Typeable, Data, Generic)

instance Hashable BandwidthStats where

makeLenses ''BandwidthStats

instance FromJSON BandwidthStats where
  parseJSON = withObject "stats bw reply" $ \o -> do
    _bwTotalIn <- o .: "TotalIn"
    _bwTotalOut <- o .: "TotalOut"
    _bwRateIn <- o .: "RateIn"
    _bwRateOut <- o .: "RateOut"
    pure BandwidthStats{..}

class HasMultihash a where
  multihash :: Lens' a Multihash
instance HasMultihash ObjectStat where
  multihash = oStatHash
instance HasMultihash BlockStat where
  multihash = bStatHash
instance HasMultihash FileStat where
  multihash = fStatHash
instance HasMultihash ObjectLink where
  multihash = oLinkHash
instance HasMultihash FileLink where
  multihash = fLinkHash

class AsText a where
  text :: Iso' a Text
instance AsText Multiaddr where
  text = iso unMultiaddr Multiaddr
instance AsText Multihash where
  text = iso unMultihash Multihash
instance AsText PeerID where
  text = iso unPeerID PeerID
