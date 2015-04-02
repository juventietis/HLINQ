module Query where

import qualified HDB.Fields as F
import qualified HDB.Tables as T
-- import qualified HDB.Connect as C
import Database.HaskellDB
import Database.HaskellDB.HDBRec
import Database.HaskellDB.HDBC
import Database.HaskellDB.Sql.SQLite
import Database.HaskellDB.HDBC.SQLite3
import Database.HaskellDB.Database
import Database.HaskellDB.PrintQuery
import Database.HDBC.Sqlite3 (connectSqlite3)

withDB :: (Database -> IO a) -> IO a
withDB = sqliteConnect "test.db"


simpleSelection = do
	table T.couples

peopleSelection = do 
	table T.people

getCouples = withDB $ \db -> query db simpleSelection
getPeople = withDB $ \db -> query db peopleSelection 


-- fun x = withDB (\db -> query db) x

projectName = withDB $ \db -> query db simpleProjection
projectNameAge = withDB $ \db -> query db simpleProjection2

simpleProjection = do
  people <- table T.people
  project $ F.name << people!F.name

simpleProjection2 = do
  people <- table T.people
  project $ F.name    << people!F.name
          # F.age << people!F.age


restrictEdna = withDB $ \db -> query db simpleRestriction

simpleRestriction = do
  people <- table T.people
  restrict $ people!F.name .==. constant "Edna"
  return people          

range lb ub = withDB $ (\db -> query db (rangeInner lb ub))  

rangeInner lb ub = do
  people <- table T.people
  restrict $ people!F.age .>=. (constant lb) .&&. people!F.age .<. (constant ub)
  project $ F.name << people!F.name

differences = withDB $ \db -> query db differencesInner

differencesInner = do 
	c <- table T.couples
	m <- table T.people
	w <- table T.people
	restrict $ c!F.her .==. w!F.name .&&. c!F.him .==. m!F.name .&&. w!F.age .>. m!F.age
	project $ F.name << w!F.name
			# F.age << (w!F.age .-. m!F.age)


getAge name = withDB $ (\db -> query db (getAgeInner name)) 

getAgeInner name = do
	p <- table T.people
	restrict $ p!F.name .==. constant name
	project $ F.age << p!F.age

compose' name1 name2 = withDB $ (\db -> query db (composeInner' name1 name2))  

composeInner' name1 name2 = do
	p <- table T.people
	restrict $ p!F.name .==. constant name1
	p2 <- table T.people
	restrict $ p2!F.name .==. constant name2
	people <- table T.people
	restrict $ people!F.age .>=. p!F.age .&&. people!F.age .<. p2!F.age
	project $ F.name << people!F.name


compose name1 name2 = withDB $ (\db -> query db (composeInner name1 name2))  


composeInner name1 name2 = do
	age1 <- getAgeInner name1
	age2 <- getAgeInner name2
	people <- table T.people
	restrict $ people!F.age .>=. age1!F.age .&&. people!F.age .<. age2!F.age
	project $ F.name << people!F.name