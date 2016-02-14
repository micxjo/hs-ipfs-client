# hs-ipfs-client

A Haskell client for the [IPFS](https://github.com/ipfs/ipfs) router api.

## Examples

```haskell
{-# LANGUAGE OverloadedStrings #-}
import           Prelude hiding (putStr, putStrLn)
import           Control.Monad.Trans
import           Control.Monad.Trans.Either
import           Data.Text (pack)
import           Data.Text.IO (putStr, putStrLn)
import           Control.Lens
import qualified Data.Vector as V
import           Data.Monoid ((<>))
import           Control.Monad (void)

import           Network.IPFS.Client

main :: IO ()
main = void $ runEitherT $ do
  v <- getVersion
  liftIO $ putStr "Router version: "
  liftIO $ case v ^. commit of
    Nothing -> putStrLn (v ^. version)
    Just commitText -> putStrLn ((v ^. version) <> "-" <> commitText)

  localID <- getLocalIdentity
  liftIO $ putStrLn ("Peer ID: " <> localID ^. peerID . peerHash)
  liftIO $ putStrLn ("Protocol Version: " <> localID ^. protocolVersion)

  addrs <- getLocalAddrs
  liftIO $ putStrLn "\nLocal addresses:"
  liftIO $ V.forM_ addrs (putStrLn . view multiaddr)

  let objHash = Multihash "QmRyWyKWmphamkMRnJVjUTzSFSAAZowYP4rnbgnfMXC9Mr"
  objStat <- getObjectStat objHash
  liftIO $ putStrLn "\nObject Stat:"
  liftIO $ putStrLn ("Hash: " <> objHash ^. multihash)
  liftIO $ putStrLn ("Num links: " <> pack (show (objStat ^. numLinks)))
  liftIO $ putStrLn ("Data size: " <> pack (show (objStat ^. dataSize)))
```
