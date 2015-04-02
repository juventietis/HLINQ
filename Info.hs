{-# LANGUAGE TemplateHaskell#-}
module Info (createDB, DBInfo (DBInfo)) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import System.IO.Unsafe

import Info.Internal

{-#NOINLINE createDB #-} 
createDB :: String -> String -> Q [Dec]
createDB dbPath dbName = do
	let info = unsafePerformIO $ getDBInfo dbPath
	--runIO $ print info
	let tableNames = extractTableNames info
	tableRecords <- createTableRecords info
	--runIO $ print tableRecords
	dbInfoRecord <- createDBInfoRecord
	--runIO $ print dbInfoRecord
	dbInfoInstance <- createDBInfoInstance dbName info
	--runIO $ print dbInfoInstance
	dbRecord <- createDBRecord dbName tableNames
	--runIO $ print dbRecord
	dbInstance <- createDBInstance dbName tableNames
	-- runIO $ print dbInstance
	dbPathVar <- return $ ValD (VarP $ mkName "dbPath") (NormalB (LitE (StringL  dbPath))) []
	declFromDB <- defFromDB dbName dbPath
	declFromDBTyped <- defFromDBTyped dbName dbPath
	-- declFromDB' <- defFromDB' dbName dbPath
	-- return $ dbInstance : dbRecord : dbInfoRecord : dbPathVar : dbInfoInstance : declFromDB : declFromDB' : tableRecords
	return $ dbInstance : dbRecord : dbPathVar : dbInfoInstance : declFromDB : declFromDBTyped : tableRecords
	--return $ {-dbInstance :-} dbRecord : dbInfoRecord : dbPathVar : dbInfoInstance : declFromDB : tableRecords
	--return $ [dbInfoInstance]

