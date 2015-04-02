{-# LANGUAGE FlexibleInstances #-}
module Database.HaskellDB.Extra where

import Database.HaskellDB           hiding (union,query,insert,update,delete)
import Database.HaskellDB.HDBRec
import Database.HaskellDB.PrimQuery
import Database.HaskellDB.Query
import System.Time

-- | Order the query by an arbitrary expression. Be careful with it.
descExpr :: Expr a -> OrderExpr
descExpr e = OrderExpr OpDesc expr where
  Expr expr = e

-- | Project a single expression (with an associated field).
projectField :: (FieldTag f,ProjectExpr e) => Attr f a -> e a -> Query (Expr a)
projectField f expr =
  fmap (!f) (project (f << expr))

-- | Construct a constant expression.
val :: (ShowConstant a) => a -> Database.HaskellDB.Query.Expr a
val = constant
