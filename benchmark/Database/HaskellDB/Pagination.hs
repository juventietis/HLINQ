{-# LANGUAGE RecordWildCards #-}

-- | Pagination utilities.

module Database.HaskellDB.Pagination where

import Database.HaskellDB.Query

import Data.Pagination

-- | Paginate a query.
paginate :: Pagination -> Query ()
paginate Pagination{..} = do
  offset (fromIntegral $ (pnCurrentPage-1) * pnPerPage)
  top (fromIntegral pnPerPage)
