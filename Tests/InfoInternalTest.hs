{-# LANGUAGE TemplateHaskell#-}
module InfoInternalTest where
import Test.Tasty
import Test.Tasty.HUnit
import Info.Internal
import Database.HDBC
import Database.HDBC.Sqlite3

convTypeTests = [testCase "Converting SqlCharT to Haskell type" ((convType SqlCharT) @?= ''Char),
				testCase "Converting SqlVarCharT to Haskell type" ((convType SqlVarCharT) @?= ''String),
				testCase "Converting SqlLongVarCharT to Haskell type" ((convType SqlLongVarCharT) @?= ''String),
				testCase "Converting SqlNumericT to Haskell type " ((convType SqlNumericT) @?= ''Integer),
				testCase "Converting SqlIntegerT to Haskell type " ((convType SqlIntegerT) @?= ''Int),
				testCase "Converting SqlIntegerT to Haskell type " ((convType SqlBigIntT) @?= ''Int)
				]

getDbInfoTest = [testCase "getDBInfo" (do
	info <- getDBInfo "test.db" 
	info @?= [("couples",[("her",SqlVarCharT),("him",SqlVarCharT)]),("people",[("name",SqlVarCharT),("age",SqlIntegerT)])])]

infoInternalUnitTests :: TestTree
infoInternalUnitTests = testGroup "Info.Internal unit tests" (convTypeTests ++ getDbInfoTest)