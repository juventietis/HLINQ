{-# OPTIONS -Wall -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-matches #-}
{-# LANGUAGE TemplateHaskell, ViewPatterns #-}

-- | Helpful template haskell utilities. Mostly a re-write of the
-- subset of Justin Bailey's haskelldb-th that was needed, as an
-- educational exercise.

module Database.HaskellDB.TH where

import Control.Monad
import Database.HaskellDB.DBLayout
import Language.Haskell.TH

-- | Generate a HaskellDB table from a list of fields.
-- Generates, e.g.
-- table :: Table (RecCons Foo (Expr Int) (RecCons Bar (Expr String) RecNil))
-- table = baseTable "xyz" (hdbMakeEntry Foo # hdbMakeEntry Bar)
table :: String
      -> String
      -> [Name]
      -> Q [Dec]
table name schemaname fields = do
  body <- [| baseTable $(nameString) $(expand fields) |]
  typ <- genSig fields
  return [sig typ,def body]

  where nameString = return $ LitE $ StringL schemaname
        sig typ = SigD tableName typ
        def body = ValD (VarP tableName) (NormalB body) []
        tableName = mkName name
        expand [] = error "expand"
        expand (name:fields) = do
          (info,typ) <- nameAndTyp name
          let expr = getCons typ
          case fields of
            []     -> [| hdbMakeEntry $(return expr) |]
            fields -> [| hdbMakeEntry $(return expr) # $(expand fields) |]
        getCons (TyConI (DataD _ _ _ [NormalC typ _] _)) = ConE typ
        getCons _ = error "getCons"

-- | Generate a type signature from a list of field types.
-- Example: Table (RecCons Foo (Expr Int) (RecCons Bar (Expr String) RecNil))
genSig :: [Name] -> Q Type
genSig fields = [t| Table $(foldM cons nil (reverse fields)) |]
  where cons acc name = do
          (info,typ) <- nameAndTyp name
          [t| RecCons $(getConsT typ) (Expr $(getFundType info)) $(return acc) |]
        getConsT (TyConI (DataD _ typ _ _ _)) = return $ ConT $ typ
        getConsT _ = error "getConsT"
        getFundType (VarI _var (getFundSubType -> cons) _ _) = return cons
        getFundType _ = error "getFundType"
        getFundSubType (AppT _ cons) = cons
        getFundSubType _ = error "getFundSubType"
        nil = ConT ''RecNil

-- | Get the info of a type and the info about its constructor.
nameAndTyp :: Name -> Q (Info,Info)
nameAndTyp name = do
  info <- reify name
  typ <- reify (getTypeNameFromInfo info)
  return (info,typ)

  where getTypeNameFromInfo (VarI _var (getType -> cons) _ _) = cons
        getTypeNameFromInfo _ = error "getTypeNameFromInfo"
        getType (AppT (AppT _ (ConT cons)) _) = cons
        getType _ = error "getType"

-- | Define a HaskellDB field.
field :: String -- ^ Defines: data TypeName = TypeName
                --   Defines: instance FieldTag TypeName where ...
      -> String -- ^ Defines: varName :: Attr TypeName String
                --            varName = mkAttr Typename
      -> String -- ^ Defines: method: fieldName _ = "example"
      -> TypeQ -- ^ Defines: varName :: Attr TypeName ColType
      -> Q [Dec]
field (mkName -> typeName) (mkName -> varName) colName colTypeQ = do
    colType <- colTypeQ
    return [dataDef,instanceDef,valSig colType,valDef] where
    dataDef = DataD context typeName [] constructors derives
        where constructors = [NormalC typeName []]
              derives = []

    instanceDef = InstanceD context (className `AppT` constrName) [method]
        where className = con ''FieldTag
              method = FunD 'fieldName [Clause [WildP] body typeAnn]
              body = NormalB $ LitE $ StringL colName

    valSig colType = SigD varName $ attrType `AppT` constrName `AppT` colType
        where attrType = con ''Attr

    valDef = VarP varName `ValD` body $ typeAnn
        where body = NormalB  $ VarE 'mkAttr `AppE` ConE typeName

    constrName = con typeName
    con = ConT
    context = []
    typeAnn = []

printQ :: (Ppr a) => Q a -> IO ()
printQ f = do
  s <- runQ f
  putStrLn $ pprint s
