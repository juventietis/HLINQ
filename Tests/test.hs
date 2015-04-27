import Test.Tasty
import Test.Tasty.HUnit
import InfoTest(infoUnitTests)
import InfoInternalTest(infoInternalUnitTests)
import QueryTests(queryUnitTests)
import UtilitiesTests(utilitiesTests)
import DeconstructorTests(deconstructorTests)

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [infoUnitTests, infoInternalUnitTests, queryUnitTests, utilitiesTests, deconstructorTests]
