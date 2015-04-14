module FillDB where

import Database.HDBC
import Database.HDBC.Sqlite3
import System.Random
import Control.Monad


fill = do
	gen <- getStdGen 
	males <- replicateM 50 $ mkPeople maleNames gen
	females <- replicateM 50 $ mkPeople femaleNames gen 
	let couples = zipWith (\x y -> [x!!0, y!!0]) females males
	--conn <- connectPostgreSQL []
	conn <- connectSqlite3 "test.db"
	quickQuery' conn "DROP TABLE IF EXISTS people" []
	quickQuery' conn "DROP TABLE IF EXISTS couples" []
	quickQuery' conn "CREATE TABLE people (name TEXT, age INT)" []
	quickQuery' conn "CREATE TABLE couples (her TEXT, him text)" []
	insPeople <- prepare conn "INSERT INTO people VALUES ((?), (?))"
	executeMany insPeople males
	executeMany insPeople females
	insCouples <- prepare conn "INSERT INTO couples VALUES ((?), (?))"
	executeMany insCouples couples
	-- newStdGen 
	-- let male2 = mkPerson maleNames gen
	commit conn
	res <-	quickQuery' conn "SELECT * FROM people LIMIT 10" []
	disconnect conn
	return res
	-- let males = take 50000 $ 

addTestTable = do
	conn <- connectSqlite3 "test.db"
	quickQuery' conn "CREATE TABLE temp (temp2 TEXT)" []
	commit conn
	disconnect conn

removeTestTable = do
	conn <- connectSqlite3 "test.db"
	quickQuery' conn "DROP TABLE IF EXISTS temp" []
	commit conn
	disconnect conn

maleNames = ["Tom", "John", "Ron", "Harry", "Mark", "Fred", "Richard", "Robin", "Christian", "Ben",  "Bert", "Drew"]

femaleNames =["Laura", "Cindy", "Lisa", "Louis", "Jeniffer", "Catherine", "Kate", "Carol", "Jane", "Alex", "Cora", "Edna"]


age gen = fst $ randomR (18,80) gen :: Int

mkPerson names gen = [toSql (names !! ((fst (randomR (0, 11) gen))::Int)), toSql $ (age gen)]
mkPeople names gen = do 
	gen <- newStdGen
	return $ mkPerson names gen