module Builder (testSuite) where

import qualified Instrumented.Data.ByteString.Builder.Tests
import qualified Instrumented.Data.ByteString.Builder.Prim.Tests
import           Test.Tasty (TestTree, testGroup)

testSuite :: TestTree
testSuite = testGroup "Builder"
  [ testGroup "Instrumented.Data.ByteString.Builder"
       Instrumented.Data.ByteString.Builder.Tests.tests

  , testGroup "Instrumented.Data.ByteString.Builder.BasicEncoding"
       Instrumented.Data.ByteString.Builder.Prim.Tests.tests
  ]
