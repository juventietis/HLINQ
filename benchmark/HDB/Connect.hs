module HDB.Connect where

import Database.HaskellDB.HDBC
import Database.HaskellDB.Sql.SQLite
import Database.HaskellDB.HDBC.SQLite3
import Database.HaskellDB.Database
import Database.HDBC.Sqlite3 (connectSqlite3)

withDB :: (Database -> IO a) -> IO a
withDB = sqliteConnect "test.db"


