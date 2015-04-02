-- | Utility for getting values from the DB with HaskellDB.

module Database.HaskellDB.Get
  (getValueWith
  ,GetValue(..)
  ,GetInstances(..)) where

import Database.HaskellDB.Database (GetValue(..),GetInstances(..))

-- | Get a value from the database with the given parser and label.
getValueWith
  :: (Show b,GetValue b)
  => (b -> Maybe a) -- ^ Parser.
  -> String -- ^ Label.
  -> (GetInstances s -> s -> String -> IO a) -- ^ GetValue method.
getValueWith get name =
  \fs s f -> do
    value <- getValue fs s f
    case get value of
      Just ok -> return ok
      Nothing -> fail $ "GetValue.getValueWith." ++ name ++
                        ": Unable to parse: " ++ f ++ " -> " ++ show value
