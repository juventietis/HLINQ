{-# LANGUAGE TemplateHaskell #-}

-- | Database tables and entities.

module HDB.Tables where

import HDB.Fields as Fields

import Database.HaskellDB.TH
import Prelude ()

-- | Content table.
table "couples" "couples"
  ['him
  ,'her
  ]

table "people" "people"
	['name,
	'age
	]