# hs-ipfs-client

A Haskell client for the [IPFS](https://github.com/ipfs/ipfs) router api.

## Examples

```haskell
{-# LANGUAGE OverloadedStrings #-}
import           Prelude hiding (putStr, putStrLn)
import           Control.Monad.Trans
import           Data.Text (pack)
import           Data.Text.IO (putStr, putStrLn)
import           Control.Lens
import qualified Data.Vector as V
import           Data.Monoid ((<>))
import           Control.Monad (void)

import           Network.IPFS

main :: IO ()
main = void $ runIPFS "localhost" 5001 $ do
  v <- getVersion
  liftIO $ putStr "Router version: "
  liftIO $ case v ^. versionCommit of
    Nothing -> putStrLn (v ^. versionText)
    Just commitText -> putStrLn ((v ^. versionText) <> "-" <> commitText)

  localID <- getLocalIdentity
  liftIO $ putStrLn ("Peer ID: " <> localID ^. pidID . text)
  liftIO $ putStrLn ("Protocol Version: " <> localID ^. pidProtocolVersion)

  addrs <- getLocalAddrs
  liftIO $ putStrLn "\nLocal addresses:"
  liftIO $ V.forM_ addrs (\a -> putStrLn (a ^. text))

  let objHash = Multihash "QmRyWyKWmphamkMRnJVjUTzSFSAAZowYP4rnbgnfMXC9Mr"
  objStat <- getObjectStat objHash
  liftIO $ putStrLn "\nObject Stat:"
  liftIO $ putStrLn ("Hash: " <> objHash ^. text)
  liftIO $ putStrLn ("Num links: " <> pack (show (objStat ^. oStatNumLinks)))
  liftIO $ putStrLn ("Data size: " <> pack (show (objStat ^. oStatDataSize)))
```
