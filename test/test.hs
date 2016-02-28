{-# LANGUAGE OverloadedStrings #-}

import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Test.Tasty
import           Test.Tasty.HUnit

import           Network.IPFS.Client

runApiary :: IPFS a -> IO (Either IPFSError a)
runApiary = runBareIPFS "private-7fa49-micxjo.apiary-mock.com" 80

assertRequest :: (Eq a, Show a) => IPFS a -> a -> Assertion
assertRequest req expected = do
  Right res <- runApiary req
  res @?= expected

apiaryTests :: TestTree
apiaryTests = testGroup "Apiary Tests"
  [ testCase "getVersion" $
      assertRequest getVersion (Version "0.4.0-dev" Nothing)

  , testCase "getBootstrapList" $
      assertRequest getBootstrapList expectedBootstrapList

  , testCase "getBlock" $ do
      let mh = Multihash "QmT78zSuBmuS4z925WZfrqQ1qHaJ56DQaTfyMUF7F8ff5o"
      assertRequest (getBlock mh) "hello world"

  , testCase "getBlockStat" $ do
      let mh = Multihash "QmW2WQi7j6c7UgJTarActp7tDNikE4B2qXtFCfLPdsgaTQ"
      assertRequest (getBlockStat mh) BlockStat { _blockHash = mh
                                                , _blockSize = 55
                                                }

  , testCase "getFileList" $ do
      let mh = Multihash "QmW2WQi7j6c7UgJTarActp7tDNikE4B2qXtFCfLPdsgaTQ"
      assertRequest (getFileList mh) (expectedFileList mh)
  ]

expectedBootstrapList :: Vector Multiaddr
expectedBootstrapList = V.fromList $ map Multiaddr
  [ "/ip4/104.236.176.52/tcp/4001/ipfs/QmSoLnSGccFuZQJzRadHn95W2CrSFmZuTdDWP8HXaHca9z"
  , "/ip4/104.131.131.82/tcp/4001/ipfs/QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ"
  , "/ip4/104.236.179.241/tcp/4001/ipfs/QmSoLPppuBtQSGwKDZT2M73ULpjvfd3aZ6ha4oFGL1KrGM"
  , "/ip4/162.243.248.213/tcp/4001/ipfs/QmSoLueR4xBeUbY9WZ9xGUUxunbKWcrNFTDAadQJmocnWm"
  ,  "/ip4/128.199.219.111/tcp/4001/ipfs/QmSoLSafTMBsPKadTEgaXctDQVcqN88CNLHXMkTNwMKPnu"
  ,  "/ip4/104.236.76.40/tcp/4001/ipfs/QmSoLV4Bbm51jM9C4gDYZQ9Cy3U6aXMJDAbzgu2fzaDs64"
  , "/ip4/178.62.158.247/tcp/4001/ipfs/QmSoLer265NRgSp2LA3dPaeykiS1J6DifTC88f5uVQKNAd"
  , "/ip4/178.62.61.185/tcp/4001/ipfs/QmSoLMeWqB7YGVLJN3pNLQpmmEk35v6wYtsMGLzSr5QBU3"
  , "/ip4/104.236.151.122/tcp/4001/ipfs/QmSoLju6m7xTh3DuokvT3886QRYqxAzb1kShaanJgW36yx"
  ]

expectedFileList :: Multihash -> FileStat
expectedFileList mh = FileStat { _fileStatHash = mh
                               , _fileStatSize = 0
                               , _fileStatType = Directory
                               , _fileStatLinks = Just (V.singleton link)
                               }
  where link = FileLink { _fileLinkName = "cat.jpg"
                        , _fileLinkHash = Multihash "Qmd286K6pohQcTKYqnS1YhWrCiS4gz7Xi34sdwMe9USZ7u"
                        , _fileLinkSize = 443230
                        , _fileLinkType = File
                        }

tests :: TestTree
tests = testGroup "ipfs-client" [apiaryTests]

main :: IO ()
main = defaultMain tests
