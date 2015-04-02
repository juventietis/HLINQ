{-# LANGUAGE TemplateHaskell #-}
module HDB.Fields where

import Database.HaskellDB.TH

-- Keys.
field "Him" "him" "him" [t|String|]

-- Data fields.
field "Her" "her" "her" [t|String|]


field "Name" "name" "name" [t|String|]

field "Age" "age" "age" [t|Int|]