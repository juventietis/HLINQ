 {-# LANGUAGE TemplateHaskell #-}
module Main where
--Gets the structure of the Database before compiling main program
import Deconstructor

import Constructor

-- import HLINQ.Deconstructor
-- import HLINQ.Info
-- import HLINQ.Constructor
--import DBquery
import Database.HDBC.Sqlite3 (connectSqlite3)
import Database.HDBC
import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax
import System.IO

import qualified Language.Haskell.Pretty as Pr
-- import QQ
import System.IO.Unsafe
import Debug.Trace
import Info



$(createDB "../test.db" "test")

