{-# LANGUAGE TemplateHaskell#-}
module Info.Internal where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Database.HDBC.Sqlite3 (connectSqlite3)
import Database.HDBC
import Control.Monad	
import Data.List (intercalate)
import Data.Maybe
import Data.String.Unicode
import Data.Decimal
import Data.Int
import System.IO.Unsafe
import Deconstructor
import Constructor
import Utilities

data DBInfo = DBInfo {info :: [(String, [(String, String)])]} deriving(Eq, Show)

getDBInfo :: String -> IO [(String, [(String, SqlTypeId)])]
getDBInfo dbPath = do
	conn <- connectSqlite3 dbPath
	tables <- getTables conn
	descriptions <- mapM (describeTable conn) tables
	return $ describeDatabase tables $ getDatabaseTypes descriptions

getColumnType :: (String, SqlColDesc) -> (String, SqlTypeId)
getColumnType  (name, info) = (name, (colType info))

getColumnTypes :: [(String, SqlColDesc)] -> [(String, SqlTypeId)]
getColumnTypes columns = map getColumnType columns 

getDatabaseTypes :: [[(String, SqlColDesc)]] -> [[(String,SqlTypeId)]]
getDatabaseTypes database = map getColumnTypes database

describeDatabase :: [String] -> [[(String, SqlTypeId)]] -> [(String, [(String, SqlTypeId)])]
describeDatabase tables types = zip tables types

-- How should the db data structure look?
-- In the paper for F# it is:
-- type DB = {people :{name:String, age:Int} list,
--			  couples:{him:String, her:String} list}
--
-- In Haskell this could be represented as:
-- data DB = DB {people::[People], couples::[Couples]}
-- data People = People {name :: String, age :: Int}
-- data Couples = Couples {him::String, her::String}
-- Implemented with access in filtering example in test_db_types.hs

-- DB term necessary to allow multiple databases to be used by a single function.

extractTableNames :: [(String, [(String, SqlTypeId)])] -> [String]
extractTableNames (x:xs) = fst x : extractTableNames xs
extractTableNames [] = []

-- Given the path to the database creates a datatype
-- in the form of Db [Table]*
-- and an instance of this type by the which was given to it.


createDBInstance :: String -> [String] -> Q Dec
createDBInstance dbName fieldNames = return $ ValD (VarP name) (NormalB (AppE (AppE cons (ListE [])) (ListE []))) [] where
	name = mkName dbName
	cons = ConE $ mkName $ toTitleCase dbName

createDBRecord :: String -> [String] -> Q Dec
createDBRecord dbName tableNames = return $ DataD context name vars cons derives where
	context = []
	name = mkName $ toTitleCase dbName
	vars = []
	cons = [RecC name fields]
	fields = createFields tableNames mkDBField
	derives = [''Show]

mkDBField :: String -> VarStrictType
mkDBField name = (mkName name, NotStrict, AppT ListT $ ConT $ mkName $ toTitleCase name)::VarStrictType

createTableRecords :: [(String, [(String, SqlTypeId)])] -> Q [Dec]
createTableRecords xs = return $ map createTableRecord xs

createTableRecord :: (String, [(String, SqlTypeId)])-> Dec
createTableRecord (tableName, columnTypes) = DataD context name vars cons derives where
	context = []
	name = mkName $ toTitleCase tableName
	vars = []
	cons = [RecC name fields]
	fields = createFields columnTypes mkTableField
	derives = [''Show]


createFields :: [a] -> (a -> VarStrictType) -> [VarStrictType]
createFields xs mkField = map mkField xs 

createDBInfoRecord :: Q Dec
createDBInfoRecord = return $ DataD context name vars cons derives where
	context = []
	name = mkName "DBInfo"
	vars = []
	cons = [NormalC name fields]
	fields = [(NotStrict,AppT ListT (AppT (AppT (TupleT 2) (ConT ''String)) (AppT ListT (AppT (AppT (TupleT 2) (ConT ''String)) (ConT ''String)))))]::[StrictType]
	derives = [''Show]

createDBInfoInstance :: String -> [(String, [(String, SqlTypeId)])] -> Q Dec
createDBInfoInstance dbName xs  = return $ ValD (VarP name) (NormalB (AppE cons (ListE body))) [] where
	name = mkName (dbName ++ "Info")
	cons = ConE $ mkName "DBInfo"
	body = createTableInfoFields xs

createTableInfoFields :: [(String, [(String, SqlTypeId)])] -> [Exp]
createTableInfoFields xs = map createTableInfoField xs

createTableInfoField :: (String, [(String, SqlTypeId)]) -> Exp
createTableInfoField (tableName, columns) = TupE [TupE [LitE (StringL tableName), ListE $ createTableInfoColumns columns]]

createTableInfoColumns :: [(String, SqlTypeId)] -> [Exp]
createTableInfoColumns xs = map createTableInfoColumn xs

createTableInfoColumn :: (String, SqlTypeId) -> Exp
createTableInfoColumn (name, typ) = TupE [LitE (StringL name), LitE (StringL (showName $ convType typ))]

mkTableField :: (String, SqlTypeId) -> VarStrictType
mkTableField (name, typ) = (mkName name, NotStrict, ConT $ convType typ)::VarStrictType

convType :: SqlTypeId -> Name
convType typ = case typ of
				SqlCharT -> ''Char
				SqlVarCharT -> ''String
				SqlLongVarCharT -> ''String
				SqlWCharT -> ''Unicode
				SqlWVarCharT -> ''UString
				SqlWLongVarCharT -> ''UString
				SqlDecimalT -> ''Decimal
				SqlNumericT -> ''Integer
				SqlSmallIntT -> ''Int16 
				SqlIntegerT -> ''Int
				SqlBigIntT -> ''Int64
				SqlFloatT -> ''Float
				SqlDoubleT -> ''Double

-- defFromDB :: String -> String -> Q Dec
-- defFromDB dbName  dbPath = return $ FunD (mkName $ "from" ++ (toTitleCase dbName)) [Clause [VarP query] (NormalB (DoE [connS, letS, letValS, statS, returnS])) []] where
-- 	connS = BindS (VarP $ mkName "conn") (AppE (VarE 'connectSqlite3) (LitE (StringL dbPath)))
-- 	letS = LetS [ValD (VarP $ mkName "convQuery") (NormalB (AppE (VarE 'expQToSQL) (VarE query))) []]
-- 	letValS = LetS [ValD (VarP $ mkName "getVals") (NormalB (AppE (VarE 'toSQLVals) (AppE (VarE 'getQueryParameters) (VarE $ mkName "convQuery")))) []]
-- 	--printS = NoBindS (AppE (VarE 'runIO) (AppE (VarE 'print) (VarE $ mkName "query")))
-- 	statS = BindS (VarP $ mkName "stat") (AppE (AppE (VarE 'prepare) (VarE $ mkName "conn")) ((AppE (VarE 'show) (VarE $ mkName "convQuery"))))
-- 	returnS = NoBindS (AppE (AppE (AppE (VarE 'execute) (VarE $ mkName "conn")) (AppE (VarE 'show) (VarE $ mkName "convQuery"))) (VarE $ mkName "getVals"))
-- 	query = mkName "query"

defFromDB :: String -> String -> Q Dec
defFromDB dbName  dbPath = return $ FunD (mkName $ "from" ++ (toTitleCase dbName)) [Clause [VarP query] (NormalB (DoE [connS, letS, letValS, statS, execS, returnS])) []] where
	connS = BindS (VarP $ mkName "conn") (AppE (VarE 'connectSqlite3) (LitE (StringL dbPath)))
	letS = LetS [ValD (VarP $ mkName "convQuery") (NormalB (AppE (VarE 'expQToSQL) (VarE query))) []]
	letValS = LetS [ValD (VarP $ mkName "getVals") (NormalB (AppE (VarE 'toSQLVals) (AppE (VarE 'getQueryParameters) (VarE $ mkName "convQuery")))) []]
	--printS = NoBindS (AppE (VarE 'runIO) (AppE (VarE 'print) (VarE $ mkName "query")))
	statS = BindS (VarP $ mkName "stat") (AppE (AppE (VarE 'prepare) (VarE $ mkName "conn")) ((AppE (VarE 'show) (VarE $ mkName "convQuery"))))
	execS = NoBindS (AppE (AppE (VarE 'execute) (VarE $ mkName "stat")) (VarE $ mkName "getVals"))
	returnS = NoBindS (AppE (VarE 'fetchAllRowsMap') (VarE $ mkName "stat"))
	query = mkName "query"

defFromDBTyped :: String -> String -> Q Dec
defFromDBTyped dbName  dbPath = return $ FunD (mkName $ "from" ++ (toTitleCase dbName) ++ "Typed") [Clause [VarP query] (NormalB (DoE [connS, letS, letValS, returnS])) []] where
	connS = BindS (VarP $ mkName "conn") (AppE (VarE 'connectSqlite3) (LitE (StringL dbPath)))
	letS = LetS [ValD (VarP $ mkName "convQuery") (NormalB (AppE (VarE 'expQToSQL) (AppE (VarE 'unTypeQ) (VarE query)))) []]
	letValS = LetS [ValD (VarP $ mkName "getVals") (NormalB (AppE (VarE 'toSQLVals) (AppE (VarE 'getQueryParameters) (VarE $ mkName "convQuery")))) []]
	--printS = NoBindS (AppE (VarE 'runIO) (AppE (VarE 'print) (VarE $ mkName "query")))
	returnS = NoBindS (AppE (AppE (AppE (VarE 'quickQuery) (VarE $ mkName "conn")) (AppE (VarE 'show) (VarE $ mkName "convQuery"))) (VarE $ mkName "getVals"))
	query = mkName "query"

--defFromDB' :: String -> String -> Q Dec
--defFromDB' dbName  dbPath = return $ FunD (mkName $ "from" ++ (toTitleCase dbName) ++ "'") [Clause [VarP query] (NormalB (DoE [connS, letS, letValS, returnS])) []] where
--	connS = BindS (VarP $ mkName "conn") (AppE (VarE 'connectSqlite3) (LitE (StringL dbPath)))
--	letS = LetS [ValD (VarP $ mkName "convQuery") (NormalB (AppE (VarE 'expQToSQL) (VarE query))) []]
--	letValS = LetS [ValD (VarP $ mkName "getVals") (NormalB (AppE (VarE 'toSQLVals) (AppE (VarE 'getQueryParameters) (VarE $ mkName "convQuery")))) []]
--	--printS = NoBindS (AppE (VarE 'runIO) (AppE (VarE 'print) (VarE $ mkName "query")))
--	letResults = LetS [ValD (Varp $ mkName "results") (NormalB (AppE (AppE (AppE (VarE 'quickQuery) (VarE $ mkName "conn")) (AppE (VarE 'show) (VarE $ mkName "convQuery"))) (VarE $ mkName "getVals"))) []]
--	returnS = NoBindS (AppE (VarE 'return) (VarE )
--	query = mkName "query"

toSQLVals :: [ValueExpr] -> [SqlValue]
toSQLVals vals = map toSQLVal vals

toSQLVal :: ValueExpr -> SqlValue
toSQLVal (NumLit val) = toSql (val::Integer)
toSQLVal (StringLit val) = toSql val
toSQLVal _ = error "Unsupported SQL value"
