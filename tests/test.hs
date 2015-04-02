import Test.Tasty
import Test.Tasty.HUnit
import InfoTest

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [infoUnitTests]

unitTests = testGroup "Unit tests"
  [ testCase "List comparison (different length)" $
      [1, 2, 3] `compare` [1,2] @?= GT

  -- the following test does not hold
  , testCase "List comparison (same length)" $
      [1, 2, 3] `compare` [1,3,4] @?= LT
  ]