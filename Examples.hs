{-# LANGUAGE TemplateHaskell #-}
module Examples where
import Info
import Utilities
import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Syntax


createDB "test.db" "test"

range'' lb ub = [|do
	person <- people
	guard $ lb <= (age person) && (age person) < ub  
	return $ (name person)|]

range' lb ub = fromTest [|do
	person <- people
	guard $ lb <= (age person) && (age person) < ub  
	return $ (name person)|]

range = [|\lb ub -> do
	person <- people
	guard $ lb <= (age person) && (age person) < ub  
	return $ (name person)|]

rangeT :: Q (TExp (Int -> Int -> [String]))
rangeT = [||\lb ub -> do
	person <- people test
	guard $ lb <= (age person) && (age person) < ub  
	return $ (name person)||]

range1 = [|\lb ub -> do
	person <- people
	return $ (name person)|]

getAge = [|\name' -> do
	person <- people
	guard $ (name person) == name' 
	return $ (age person)|]

getAgeT :: Q (TExp (String -> [Int]))
getAgeT = [||\name' -> do
	person <- people test
	guard $ (name person) == name' 
	return $ (age person)||]



getAge1 = [|\name' -> do
	person <- people
	return $ (age person)|]


getAge' name' = fromTest [|do
	person <- people
	guard $ (name person) == name' 
	return $ (age person)|]

getAge'' = [|\name' -> do
	person <- people
	guard $ (name person) == name' 
	return $ (age person)|]

-- compose'' = [|\name1 name2 -> do
-- 	age1 <- (\name' -> do
-- 		person <- people
-- 		guard $ (name person) == name' 
-- 		return $ (age person)) name1
-- 	age2 <- (\name' -> do
-- 		person <- people
-- 		guard $ (name person) == name' 
-- 		return $ (age person)) name2
-- 	(\lb ub -> do
-- 	person <- people
-- 	guard $ lb <= (age person) && (age person) < ub  
-- 	return $ (name person)) age1 age2|]

compose = [|\name1 name2 -> do
	age1 <- $(getAge) name1
	age2 <- $(getAge) name2
	$(range) age1 age2|]

composeT = [||\name1 name2 -> do
	age1 <- $$(getAgeT) name1
	age2 <- $$(getAgeT) name2
	$$(rangeT) age1 age2||]

differences = [|do
	c <- couples
	m <- people
	w <- people
	guard $ ((her c) == (name w)) && ((him c) == (name m)) && ((age w) > (age m))
	return $ ((name w), (age w) - (age m))|]


differencesT = [||do
	c <- couples test
	m <- people test
	w <- people test
	guard $ ((her c) == (name w)) && ((him c) == (name m)) && ((age w) > (age m))
	return $ ((name w), (age w) - (age m))||]

-- differencesTErr = [||do
-- 	c <- couples test
-- 	m <- people test
-- 	w <- people test
-- 	guard $ ((her c) == (age w)) && ((him c) == (name m)) && ((age w) > (age m))
-- 	return $ ((name w), (age w) - (age m))||]	

satisfies = [|\p -> do person <- people; guard $ (p (age person)); return $ (name person)|]

satisfiesT = [||\p -> do 
	person <- people test
	guard $ (p (age person))
	return $ (name person)||]

pre = [|(\x -> 30 < x && x <= 40)|]

preT :: Q(TExp (Int->Bool))
preT = [||(\x -> 30 < x && x <= 40)||]
preT2 = [||(\x -> x !! 1)||]

data Predicate = Above Int |
				 Below Int |
				 And Predicate Predicate |
				 Or Predicate Predicate |
				 Not Predicate deriving (Eq, Show)

p :: Predicate -> Q(TExp (Int -> Bool))
p (Above n) = [||\x -> n <= x||]
p (Below n) = [||\x -> n > x||]
p (And n t) = [||\x -> $$(p n) x && $$(p t) x||]
p (Or n t) = [||\x -> $$(p n) x || $$(p t) x||]
p (Not n) = [||\x -> not ($$(p n) x)||]

t0 = And (Above 30) (Below 40)

satisfiesTD = [||$$satisfiesT $$(p t0)||]
a:: Int
a = 30
b:: Int
b = 40

rangeTT b= [||$$rangeT ($$((unsafeTExpCoerce $ lift a))) b||]
rangeTT2= [||$$rangeT $$(liftLinq a) $$(liftLinq b)||]

rangeTTT a b=  [||$$rangeT a b||]
