{-# LANGUAGE TemplateHaskell#-}
module Utilities where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Char (toUpper, toLower)
import Database.HDBC
import Data.Convertible

liftLinq  a = unsafeTExpCoerce $ lift a



toTitleCase :: String -> String
toTitleCase [] = ""
toTitleCase [x] = [toUpper x]
toTitleCase (x:xs) = toUpper x : xs

toRecord toRec con valsIO  = do
	vals <- valsIO
	let conv = toRec con vals
	return conv

toRec con vals = map (\x -> con (fromSql $ x!!0)) vals
toRec2 con vals = map (\x -> con (fromSql $ x!!0) (fromSql $ x!!1)) vals
toRec3 con vals = map (\x -> con (fromSql $ x!!0) (fromSql $ x!!1) (fromSql $ x!!2)) vals
toRec4 con vals = map (\x -> con (fromSql $ x!!0) (fromSql $ x!!1) (fromSql $ x!!2) (fromSql $ x!!3)) vals
toRec5 con vals = map (\x -> con (fromSql $ x!!0) (fromSql $ x!!1) (fromSql $ x!!2) (fromSql $ x!!3) (fromSql $ x!!4)) vals
toRec6 con vals = map (\x -> con (fromSql $ x!!0) (fromSql $ x!!1) (fromSql $ x!!2) (fromSql $ x!!3) (fromSql $ x!!4) (fromSql $ x!!5)) vals
toRec7 con vals = map (\x -> con (fromSql $ x!!0) (fromSql $ x!!1) (fromSql $ x!!2) (fromSql $ x!!3) (fromSql $ x!!4) (fromSql $ x!!5) (fromSql $ x!!6)) vals
toRec8 con vals = map (\x -> con (fromSql $ x!!0) (fromSql $ x!!1) (fromSql $ x!!2) (fromSql $ x!!3) (fromSql $ x!!4) (fromSql $ x!!5) (fromSql $ x!!6) (fromSql $ x!!7)) vals
toRec9 con vals = map (\x -> con (fromSql $ x!!0) (fromSql $ x!!1) (fromSql $ x!!2) (fromSql $ x!!3) (fromSql $ x!!4) (fromSql $ x!!5) (fromSql $ x!!6) (fromSql $ x!!7) (fromSql $ x!!8)) vals
toRec10 con vals = map (\x -> con (fromSql $ x!!0) (fromSql $ x!!1) (fromSql $ x!!2) (fromSql $ x!!3) (fromSql $ x!!4) (fromSql $ x!!5) (fromSql $ x!!6) (fromSql $ x!!7) (fromSql $ x!!8) (fromSql $ x!!9)) vals

toTuple toTup valsIO  = do
	vals <- valsIO
	let conv = toTup vals
	return conv

toTup2 vals = map (\x -> (fromSql $ x!!0, fromSql $ x!!1)) vals
toTup3 vals = map (\x -> (fromSql $ x!!0, fromSql $ x!!1, fromSql $ x!!2)) vals
toTup4 vals = map (\x -> (fromSql $ x!!0, fromSql $ x!!1, fromSql $ x!!2, fromSql $ x!!3)) vals
toTup5 vals = map (\x -> (fromSql $ x!!0, fromSql $ x!!1, fromSql $ x!!2, fromSql $ x!!3, fromSql $ x!!4)) vals
toTup6 vals = map (\x -> (fromSql $ x!!0, fromSql $ x!!1, fromSql $ x!!2, fromSql $ x!!3, fromSql $ x!!4, fromSql $ x!!5)) vals
toTup7 vals = map (\x -> (fromSql $ x!!0, fromSql $ x!!1, fromSql $ x!!2, fromSql $ x!!3, fromSql $ x!!4, fromSql $ x!!5, fromSql $ x!!6)) vals
toTup8 vals = map (\x -> (fromSql $ x!!0, fromSql $ x!!1, fromSql $ x!!2, fromSql $ x!!3, fromSql $ x!!4, fromSql $ x!!5, fromSql $ x!!6, fromSql $ x!!7)) vals
toTup9 vals = map (\x -> (fromSql $ x!!0, fromSql $ x!!1, fromSql $ x!!2, fromSql $ x!!3, fromSql $ x!!4, fromSql $ x!!5, fromSql $ x!!6, fromSql $ x!!7, fromSql $ x!!8)) vals
toTup10 vals = map (\x -> (fromSql $ x!!0, fromSql $ x!!1, fromSql $ x!!2, fromSql $ x!!3, fromSql $ x!!4, fromSql $ x!!5, fromSql $ x!!6, fromSql $ x!!7, fromSql $ x!!8, fromSql $ x!!9)) vals
