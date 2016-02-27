
{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit

import Network.IPFS.Client

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
  ]

tests :: TestTree
tests = testGroup "ipfs-client" [apiaryTests]

main :: IO ()
main = defaultMain tests
