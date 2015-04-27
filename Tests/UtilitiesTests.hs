module UtilitiesTests where

import Test.Tasty
import Test.Tasty.HUnit
--import Test.Tasty.SmallCheck

import Database.HDBC
import Database.HLINQ.Utilities

--rangeTest = [testCase "Range 30 40" (do
--	res <- (fromDb [||$$rangeT 30 40||])
--	resSQL <- range (30::Int) (40::Int)
--	res @?= resSQL),
--	testCase "Range 0 100" (do
--	res <- (fromDb [||$$rangeT 0 100||])
--	resSQL <- range (0::Int) (100::Int)
--	res @?= resSQL),
--	testCase "Range 0 10" (do
--	res <- (fromDb [||$$rangeT 0 10||])
--	resSQL <- range (0::Int) (10::Int)
--	res @?= resSQL)
--	]
data Rec1 = Rec1 String deriving (Show, Eq)
data Rec2 = Rec2 String String deriving (Show, Eq)
data Rec3 = Rec3 String String Int deriving (Show, Eq)
data Rec4 = Rec4 String String Int String deriving (Show, Eq)
data Rec5 = Rec5 String String Int String Char deriving (Show, Eq)
toRecTests = [testCase "toRec " (toRec Rec1 [[toSql "Hello"]] @?= [Rec1 "Hello"]),
			  testCase "toRec empty" (toRec Rec1 [] @?= []),
			  testCase "toRec2" (toRec2 Rec2 [[toSql "Hello", toSql "World"]] @?= [Rec2 "Hello" "World"]),
			  testCase "toRec3" (toRec3 Rec3 [[toSql "Hello", toSql "World", (toSql (42::Int))]] @?= [Rec3 "Hello" "World" 42]),
			  testCase "toRec4" (toRec4 Rec4 [[toSql "Hello", toSql "World", (toSql (42::Int)), toSql ""]] @?= [Rec4 "Hello" "World" 42 ""]),
			  testCase "toRec5" (toRec5 Rec5 [[toSql "Hello", toSql "World", (toSql (42::Int)), toSql "", toSql '!']] @?= [Rec5 "Hello" "World" 42 "" '!'])
			  ] 

toListTests = [testCase "toList" (do
	vals <- toList $ return [[toSql "Hello"]]
	vals @?= ["Hello"]),
	testCase "toList []" (do
	vals <- ((toList $ return []) :: IO [Int])
	vals @?= ([]::[Int]))]

toTupTests = [testCase "toTup" (toTup2 [[toSql "Hello", toSql "World!"]] @?= [("Hello", "World!")])]

utilitiesTests :: TestTree
utilitiesTests = testGroup "Utilities unit tests" (toRecTests ++ toListTests ++ toTupTests)