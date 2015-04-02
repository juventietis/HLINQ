{-# LANGUAGE TemplateHaskell#-}
module Utilities where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Char (toUpper, toLower)

liftLinq  a = unsafeTExpCoerce $ lift a



toTitleCase :: String -> String
toTitleCase [] = ""
toTitleCase [x] = [toUpper x]
toTitleCase (x:xs) = toUpper x : xs

