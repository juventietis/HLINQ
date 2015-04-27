module DeconstructorTests where

import Test.Tasty
import Test.Tasty.HUnit
import Database.HLINQ.Deconstructor
import Database.HLINQ.Constructor
import Language.Haskell.TH

--guardExpTests = [testCase "guardExp InfixE" (guardExp (InfixE (Just $ mkName "four") ('<) (Just $ mkName "two")) @?= BinOp)]

appExpTests = [testCase "appExp Select Column" (appExp (AppE (VarE $ mkName "name") (VarE $ mkName "people")) @?= DIden ("name") ("people")),
			   testCase "appExp NumLit" (appExp (LitE (IntegerL 5)) @?= NumLit 5),
			   testCase "appExp StringLit" (appExp (LitE (StringL "Hello world!")) @?= StringLit "Hello world!"),
			   testCase "appExp rebuild string " (appExp (ListE [(LitE (CharL 'a'))]) @?= StringLit "a"),
			   testCase "appExp rebuild string " (appExp (ListE [(LitE (CharL 'a')), (LitE (CharL 'b')) ,(LitE (CharL 'c'))]) @?= StringLit "abc")
			   ]

returnExpTests = [testCase "returnExp " (returnExp (TupE [(AppE (VarE $ mkName "name") (VarE $ mkName "people"))]) @?= [DIden ("name") ("people")])]


deconstructorTests = testGroup "Deconstructor Tests" (appExpTests ++ returnExpTests)