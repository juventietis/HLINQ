{-# LANGUAGE TemplateHaskell #-}
module QueryTests where
import Database.HLINQ.Info
import Database.HLINQ.Utilities
import Control.Monad
import Test.Tasty
import Test.Tasty.HUnit
import Language.Haskell.TH

import Database.HDBC
import Database.HDBC.Sqlite3
import Language.Haskell.TH.Syntax


createDB "test.db" "db"

--checkDBHashTest = [
--	testCase "Checking DB Hash" (do
--	res <- checkDbConsistency'
--	res @?= (Right "Database check passed")),
--   	testCase "Checking DB Hash after changes" (do
--	addTestTable
--	res <- checkDbConsistency'
--	removeTestTable
--	res @?= (Left "Database Structure has changed since last compilation, please recompile."))]

rangeTest = [testCase "Range 30 40" (do
	res <- (fromDb [||$$rangeT 30 40||])
	resSQL <- range (30::Int) (40::Int)
	res @?= resSQL),
	testCase "Range 0 100" (do
	res <- (fromDb [||$$rangeT 0 100||])
	resSQL <- range (0::Int) (100::Int)
	res @?= resSQL),
	testCase "Range 0 10" (do
	res <- (fromDb [||$$rangeT 0 10||])
	resSQL <- range (0::Int) (10::Int)
	res @?= resSQL)
	]

rangeT :: Q (TExp (Int -> Int -> [String]))
rangeT = [||\lb ub -> do
	person <- people db
	guard $ lb <= (age person) && (age person) < ub  
	return $ (name person)||]

range a b = do
	conn <- connectSqlite3 "test.db"
	res <- quickQuery' conn "SELECT name FROM people WHERE (?) <= age AND age < (?)" [(toSql a), (toSql b)]
	disconnect conn
	return res

 
ageTest = [testCase "GetAge Drew" (do
	res <- (fromDb [||$$getAgeT "Edna"||])
	resSQL <- getAge "Edna"
	res @?= resSQL
	)]

getAgeT :: Q (TExp (String -> [Int]))
getAgeT = [||\name' -> do
	person <- people db
	guard $ (name person) == name' 
	return $ (age person)||]

getAge name = do
	conn <- connectSqlite3 "test.db"
	res <- quickQuery' conn "SELECT age FROM people WHERE name = (?)" [toSql name]
	disconnect conn
	return res


differencesT = [||do
	c <- couples db
	m <- people db
	w <- people db
	guard $ ((her c) == (name w)) && ((him c) == (name m)) && ((age w) > (age m))
	return $ ((name w), (age w) - (age m))||]

differences = do
	conn <- connectSqlite3 "test.db"
	res <- quickQuery' conn "SELECT w.name, (w.age - m.age) FROM couples, people as m, people as w WHERE her = w.name AND him = m.name AND w.age > m.age" []
	disconnect conn
	return res


differencesTest = [testCase "Differences" (do
	res <- fromDb [||$$differencesT||]
	resSQL <- differences
	res @?= resSQL)]


preT :: Q(TExp (Int->Bool))
preT = [||(\x -> 30 < x && x <= 40)||]


satisfiesT = [||\p -> do 
	person <- people db
	guard $ (p (age person))
	return $ (name person)||]

satisfies = do
	conn <- connectSqlite3 "test.db"
	res <- quickQuery'  conn "SELECT name FROM people WHERE 30 < age AND age <= 40" []
	disconnect conn
	return res

satisfiesTest = [testCase "Satisfies" (do
	res <- fromDb [||$$satisfiesT $$preT||]
	resSQL <- satisfies
	res @?= resSQL)]

composeT = [||\name1 name2 -> do
	age1 <- $$(getAgeT) name1
	age2 <- $$(getAgeT) name2
	$$(rangeT) age1 age2||]

compose name1 name2 = do
	conn <- connectSqlite3 "test.db"
	res <- quickQuery' conn "SELECT p.name FROM people as p1, people as p2, people as p WHERE p1.name = (?) AND p2.name = (?) AND p1.age <= p.age AND p.age < p2.age" [toSql name1, toSql name2]
	disconnect conn
	return res

composeTest = [testCase "Compose Edna Drew" (do
	res <- fromDb [||$$composeT "Edna" "Drew"||]
	resSQL <- compose "Edna" "Drew"
	res @?= resSQL)]


queryUnitTests :: TestTree
queryUnitTests = testGroup "Query unit tests" (rangeTest ++ ageTest ++ differencesTest ++ satisfiesTest ++ composeTest)