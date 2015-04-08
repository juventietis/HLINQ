module Constructor where

-- Change approach to the one using SQL parameters.
data ValueExpr = StringLit String
	| NumLit Integer
	| BoolLit Bool
	| Iden String
	| DIden String String -- a.b
	| Star
	| DStar String -- t.*
	| App String [ValueExpr]
	| PrefOp String ValueExpr
	| BinOp ValueExpr String ValueExpr
	| Case (Maybe ValueExpr) -- test value
		[(ValueExpr,ValueExpr)] -- when branches
		(Maybe ValueExpr) -- else value
	| Parens ValueExpr
	| Union ValueExpr ValueExpr deriving(Eq)
instance Show ValueExpr where
	show (Iden s) = s
	show (DIden s1 s2)  = s2 ++ "." ++ s1	
	show (Star) 	    =  "*"
	show (DStar s) = s ++ ".*"
	show (BinOp exp1 op exp2) = show exp1 ++ " " ++ op ++ " " ++ show exp2  
	show (Parens exp) = "(" ++ show exp ++ ")"
	show (NumLit val) = show val
	show (StringLit val) = val


getQueryParameters :: QueryExpr -> [ValueExpr]
getQueryParameters queryExp = case (qeWhere queryExp) of
									Just exp -> getValsWhere exp
									Nothing -> []

getValsWhere :: ValueExpr -> [ValueExpr]
getValsWhere (BinOp exp1 string exp2) 
  | string `elem` hsBinOpStr = case exp2 of
  									(NumLit val) -> (getValsWhere exp1) ++ [exp2]
  									(StringLit val) -> (getValsWhere exp1) ++ [exp2]
  									otherwise -> (getValsWhere exp1)
  | otherwise = getValsWhere exp1 ++ getValsWhere exp2
getValsWhere exp@(DIden _ _) = []
getValsWhere exp@(NumLit val) = [exp]
getValsWhere exp@(StringLit val) = [exp]
getValsWhere exp = error $ "getValsWhere: " ++ show exp 

knockOutValsWhere :: ValueExpr -> ValueExpr
knockOutValsWhere (BinOp exp1 string exp2) 
  | string `elem` hsBinOpStr = case exp2 of
  									(NumLit val) -> (BinOp (knockOutValsWhere exp1) string (StringLit "(?)"))
  									(StringLit val) -> (BinOp (knockOutValsWhere exp1) string (StringLit "(?)"))
  									otherwise -> (BinOp (knockOutValsWhere exp1) string exp2)
  | otherwise = BinOp (knockOutValsWhere exp1) string (knockOutValsWhere exp2)
knockOutValsWhere exp@(DIden _ _) = exp 
knockOutValsWhere exp@(NumLit val) = StringLit "(?)"
knockOutValsWhere exp@(StringLit val) = StringLit "(?)"

hsBinOpStr :: [String]
hsBinOpStr = ["=", "<", ">", "<=", ">=", "/="]

data SqlQuery = UnionAll [QueryExpr] deriving (Eq) 
instance Show SqlQuery where
	show (UnionAll a) = foldr (\x str -> str ++ " UNION ALL" ++ show x) "" a 

data QueryExpr = Select
	{qeSelectList :: [(ValueExpr,Maybe String)]
	,qeFrom :: [TableRef]
	,qeWhere :: Maybe ValueExpr
	,qeGroupBy :: [ValueExpr]
	,qeHaving :: Maybe ValueExpr
	,qeOrderBy :: [ValueExpr]
	} deriving (Eq)
instance Show QueryExpr where
	show (Select {qeSelectList=selectS, qeFrom=fromS, qeWhere=(Just whereS)}) = showSelect ++ showFrom ++ showWhere where 
		showSelect = "SELECT " ++ showAnyList (map (\(x, _) -> x) selectS)  ++ "\n"
		showFrom = "FROM " ++ showAnyList fromS  ++ "\n"
		showWhere = "WHERE " ++ (show $ knockOutValsWhere whereS)
	show (Select {qeSelectList=selectS, qeFrom=fromS, qeWhere=(Nothing)}) = showSelect ++ showFrom where 
		showSelect = "SELECT " ++ showAnyList (map (\(x, _) -> x) selectS)  ++ "\n"
		showFrom = "FROM " ++ showAnyList fromS  ++ "\n"

makeSelect :: QueryExpr
makeSelect = Select {qeSelectList = []
			,qeFrom = []
			,qeWhere = Nothing
			,qeGroupBy = []
			,qeHaving = Nothing
			,qeOrderBy = []}

data TableRef = TRSimple String
	| TRJoin TableRef JoinType TableRef (Maybe JoinCondition)
	| TRParens TableRef
	| TRAlias TableRef String
	| TRQueryExpr QueryExpr
	deriving (Eq)
instance Show TableRef where
	show (TRSimple s) = s
	show (TRAlias ref s)  = show ref ++ " AS " ++ s 

data JoinType = JoinInner | JoinLeft | JoinRight | JoinFull | JoinCross
	deriving (Eq,Show)

data JoinCondition = JoinOn ValueExpr
	| JoinUsing [String]
	| JoinNatural
	deriving (Eq,Show)



showAnyList ::(Show a) => [a] -> String
showAnyList [] = ""
showAnyList [x] = show x 
showAnyList (x:xs) = show x ++ ", " ++ (showAnyList xs)

