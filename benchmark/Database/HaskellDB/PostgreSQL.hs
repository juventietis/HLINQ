{-# LANGUAGE EmptyDataDecls #-}

-- | PostgreSQL-specific HaskellDB utilities.

module Database.HaskellDB.PostgreSQL
  (TSVector
  ,TSQuery
  ,(.@@.)
  ,to_tsquery
  ,to_tsvector
  ,ts_rank_cd)
  where
  
import Database.HaskellDB
import Database.HaskellDB.Query
import Database.HaskellDB.PrimQuery
import Database.HaskellDB.Get
import System.Time

-- | A text-search vector.
data TSVector = TSVector
instance GetValue TSVector where getValue _ _ _ = return TSVector

-- | A text-search query.
data TSQuery

-- | Operator to match a text search vector against a query.
(.@@.) :: Expr TSVector -> Expr TSQuery -> Expr Bool
(.@@.) = binop (OpOther "@@")
infix 4 .@@.

-- | Convert a string to a textsearch query.
to_tsquery :: Expr String -> Expr TSQuery
to_tsquery = func "to_tsquery"

-- | Convert a string to a textsearch vector.
to_tsvector :: Expr String -> Expr TSVector
to_tsvector = func "to_tsvector"

-- | Rank a text search query.
ts_rank_cd :: Expr TSVector -> Expr TSQuery -> Expr Double
ts_rank_cd = func "ts_rank_cd"

-- | Get part of a date.
date_part :: Expr String -> Expr CalendarTime -> Expr Integer
date_part = func "date_part"
