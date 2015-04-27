{-# LANGUAGE TemplateHaskell#-}
module InfoTest where
import Test.Tasty
import Test.Tasty.HUnit
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Int
import Database.HLINQ.Info
import Database.HLINQ.Info.Internal
import Database.HLINQ.Utilities
import FillDB(fill, addTestTable, removeTestTable)




toTitleCaseTests = [testCase "Converting lowercase string to title case" ((toTitleCase "hello") @?= "Hello"),
					testCase "Converting title case string to to title case" ((toTitleCase "Hello") @?= "Hello"),
					testCase "Converting a string of length one to title case" ((toTitleCase "h") @?= "H"),
					testCase "Converting empty string" ((toTitleCase "") @?= "")
					]



createDB "test.db" "db"

checkDBHashTest = [
	testCase "Checking DB Hash" (do
	res <- checkDbConsistency'
	res @?= (Right "Database check passed")),
   	testCase "Checking DB Hash after changes" (do
	addTestTable
	res <- checkDbConsistency'
	removeTestTable
	res @?= (Left "Database Structure has changed since last compilation, please recompile."))]




createDBTests = [testCase "Creating a db DB" (dbInfo @?= DBInfo [("couples",[("her","GHC.Base.String"),("him","GHC.Base.String")]), ("people",[("name","GHC.Base.String"),("age","GHC.Types.Int")])])]




--mkTableTypeTests = [testCase "Making a table type from name" ((mkTableType "change" SqlFloatT) @?= ())]

--createTableInfoColumnTests = [testCase "Creating table info column " ((createTableInfoColumn ("change", SqlFloatT)) @?= [|("change", "float")|])]

infoUnitTests :: TestTree
infoUnitTests = testGroup "Info unit tests" (toTitleCaseTests ++ checkDBHashTest ++ createDBTests)

