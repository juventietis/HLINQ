{-# LANGUAGE TemplateHaskell#-}
module InfoTest where
import Test.Tasty
import Test.Tasty.HUnit
import Database.HDBC
import Data.String.Unicode
import Data.Decimal
import Data.Int
import Info
import Info.Internal
import Utilities

infoUnitTests :: TestTree
infoUnitTests = testGroup "Info unit tests" (convTypeTests ++ toTitleCaseTests)




toTitleCaseTests = [testCase "Converting lowercase string to title case" ((toTitleCase "hello") @?= "Hello"),
					testCase "Converting title case string to to title case" ((toTitleCase "Hello") @?= "Hello"),
					testCase "Converting a string of length one to title case" ((toTitleCase "h") @?= "H"),
					testCase "Converting empty string" ((toTitleCase "") @?= "")
					]

convTypeTests = [testCase "Converting SqlCharT to Haskell type" ((convType SqlCharT) @?= ''Char),
				testCase "Converting SqlVarCharT to Haskell type" ((convType SqlVarCharT) @?= ''String),
				testCase "Converting SqlLongVarCharT to Haskell type" ((convType SqlLongVarCharT) @?= ''String),
				testCase "Converting SqlWCharT to Haskell type" ((convType SqlWCharT) @?= ''Unicode),
				testCase "Converting SqlWVarCharT to Haskell type" ((convType SqlWVarCharT) @?= ''UString),
				testCase "Converting SqlWLongVarCharT to Haskell type " ((convType SqlWLongVarCharT) @?= ''UString),
				testCase "Converting SqlDecimalT to Haskell type " ((convType SqlDecimalT) @?= ''Decimal),
				testCase "Converting SqlNumericT to Haskell type " ((convType SqlNumericT) @?= ''Integer),
				testCase "Converting SqlSmallIntT to Haskell type " ((convType SqlSmallIntT) @?= ''Int16),
				testCase "Converting SqlIntegerT to Haskell type " ((convType SqlIntegerT) @?= ''Int),
				testCase "Converting SqlFloatT to Haskell type " ((convType SqlFloatT) @?= ''Float),
				testCase "Converting SqlDoubleT to Haskell type " ((convType SqlDoubleT) @?= ''Double)
				--testCase "Converting SqlFloatT to Haskell type " ((convType SqlFloatT) @?= ''Float),


				]
$(createDB "test.db" "test")
createDBTests = [testCase "Creating a test DB" (testInfo @?= DBInfo [("couples",[("her","GHC.Base.String"),("him","GHC.Base.String")]),("people",[("name","GHC.Base.String"),("age","GHC.Int.Int32")])])]

--mkTableTypeTests = [testCase "Making a table type from name" ((mkTableType "change" SqlFloatT) @?= ())]

--createTableInfoColumnTests = [testCase "Creating table info column " ((createTableInfoColumn ("change", SqlFloatT)) @?= [|("change", "float")|])]


