{-# LANGUAGE TemplateHaskell #-}
module Info (createDB, DBInfo (DBInfo), info, checkDbHash', checkDbHash, expQToSQL, tExpQToSQL) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Hashable
import System.IO.Unsafe
import Deconstructor(expQToSQL, tExpQToSQL)
import Info.Internal


{-#NOINLINE createDB #-} 
createDB :: String -> String -> Q [Dec]
createDB dbPath dbName = do
	let info = unsafePerformIO $ getDBInfo dbPath
	--runIO $ print info
	let tableNames = extractTableNames info
	tableRecords <- createTableRecords info
	--runIO $ print tableRecords
	-- dbInfoRecord <- createDBInfoRecord
	--runIO $ print dbInfoRecord
	let dbInfo = createTableInfos info
	-- runIO $ print $ createTableInfos info
	dbInfoInstance <- createDBInfoInstance dbName info
	--runIO $ print dbInfoInstance
	dbRecord <- createDBRecord dbName tableNames
	--runIO $ print dbRecord
	dbInstance <- createDBInstance dbName tableNames
	-- runIO $ print dbInstance
	dbPathVar <- return $ ValD (VarP $ mkName "dbPath") (NormalB (LitE (StringL  dbPath))) []
	declFromDBUntyped <- defFromDBUntyped dbName dbPath
	declFromDB <- defFromDB dbName dbPath
	compileHash <- return $ ValD (VarP $ mkName "compileDbHash") (NormalB (LitE (IntegerL $ toInteger (hash dbInfo)))) []
	dbCheck <- return $ ValD (VarP $ mkName "checkDBConsistency") (NormalB (AppE (AppE (VarE $ mkName "checkDbHash") ((LitE (IntegerL $ toInteger (hash dbInfo))))) (LitE (StringL  dbPath)))) []
	dbCheck' <- return $ ValD (VarP $ mkName "checkDBConsistency'") (NormalB (AppE (AppE (VarE $ mkName "checkDbHash'") ((LitE (IntegerL $ toInteger (hash dbInfo))))) (LitE (StringL  dbPath)))) []
	-- declFromDB' <- defFromDB' dbName dbPath
	-- return $ dbInstance : dbRecord : dbInfoRecord : dbPathVar : dbInfoInstance : declFromDB : declFromDB' : tableRecords
	return $ dbInstance : dbRecord : dbPathVar : dbCheck : dbCheck' : dbInfoInstance : declFromDBUntyped : declFromDB : tableRecords
	--return $ {-dbInstance :-} dbRecord : dbInfoRecord : dbPathVar : dbInfoInstance : declFromDB : tableRecords
	--return $ [dbInfoInstance]

checkDbHash :: Integer -> String -> IO ()
checkDbHash compileHash dbPath = do
	info <- getDBInfo dbPath
	let dbInfoHash = toInteger $ hash $ createTableInfos info
	if dbInfoHash == compileHash then putStrLn "Database check passed" else fail "Database Structure has changed since last compilation, please recompile." 

checkDbHash' :: Integer -> String -> IO (Either String String)
checkDbHash' compileHash dbPath = do
	info <- getDBInfo dbPath
	let dbInfoHash = toInteger $ hash $ createTableInfos info
	return $ if dbInfoHash == compileHash then Right "Database check passed" else Left "Database Structure has changed since last compilation, please recompile." 

