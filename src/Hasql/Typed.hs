{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs,
             MultiParamTypeClasses, PolyKinds, ScopedTypeVariables,
             StandaloneDeriving, TemplateHaskell, TypeFamilies, TypeOperators,
             UndecidableInstances #-}
module Hasql.Typed where
import Control.Monad
--------------------------------------------------------------------------------
import Data.Int
import Data.List
import Data.Word
--------------------------------------------------------------------------------
import GHC.Exts                   (Constraint)
import GHC.TypeLits
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
--------------------------------------------------------------------------------
import qualified Hasql            as Hasql
import           Hasql.Typed.Util

newtype Database model db = Database { getPool :: Hasql.Pool db }

data Column :: Symbol -> * -> * where
  Column :: Column name hstype

deriving instance Show (Column parent name)

type family NotElem e list :: Constraint where
  NotElem e '[]                = Yes
  NotElem e (Column e t ': es) = No "duplicate name"
  NotElem e (Column x t ': xs) = NotElem e xs

type family DBType db hstype :: Symbol
type instance DBType db (Maybe a) = DBType db a

class Is f a
instance Is f (f a)

-- | actually a proxy
data NotProxy a = NotProxy

backendType :: KnownSymbol (DBType db hstype) => db -> hstype -> String
backendType (_db :: db) (_hstype :: hstype) =
  symbolVal (NotProxy :: NotProxy (DBType db hstype))

data Table db a where
  Empty :: Table db '[]
  Col :: (name `NotElem` columns)
      => Column name hstype
      -> Table db columns
      -> Table db (Column name hstype ': columns)

deriving instance Show (Table db a)

class Model db a where
  type Columns a :: [*]
  type PK a      :: [*]
  schema :: proxy a -> Hasql.Stmt db
  table  :: proxy a -> Table db (Columns a)

class BuildTable db a where
  buildTable :: Table db a

instance (name `NotElem` columns, BuildTable db columns) =>
         BuildTable db (Column name hstype ': columns) where
  {-# INLINE buildTable #-}
  buildTable = Col Column buildTable

instance BuildTable db '[] where
  {-# INLINE buildTable #-}
  buildTable = Empty

data Test

type instance DBType Test Int   = "int8"
type instance DBType Test Int16 = "int2"
type instance DBType Test Int32 = "int4"
type instance DBType Test Int64 = "int8"

type instance DBType Test Word   = "int8"
type instance DBType Test Word8  = "byte"
type instance DBType Test Word16 = "int2"
type instance DBType Test Word32 = "int4"
type instance DBType Test Word64 = "int8"

type instance DBType Test String = "text"

type family PkOf orig t acc where
  PkOf orig (Column name (PrimaryKey a) ': xs) acc = PkOf orig xs (name ': acc)
  PkOf orig '[]                                '[] = TryUniques orig orig '[]

type family TryUniques orig l acc where
  TryUniques orig '[]                            '[] = Names orig
  TryUniques orig (Column name (Unique a) ': xs) acc = '[name]

type family Names a where
  Names (Column name a ': xs) = name ': Names xs
  Names '[]                   = '[]

newtype References :: * -> Symbol -> * -> * where
  References :: a -> References parent column a

newtype PrimaryKey a = PrimaryKey a
newtype Unique     a = Unique a

class IsModifier a
instance IsModifier (References parent col)
instance IsModifier PrimaryKey
instance IsModifier Unique

removeModifiers :: Dec -> Q Dec
removeModifiers d = case d of
  DataD cxt' typ tyvars constrs derivs -> fmap
    (\constrs' -> DataD cxt' typ tyvars constrs' derivs)
    (mapM procMods constrs)
  a -> return a
 where
  setStrict Unpacked = Unpacked
  setStrict _        = IsStrict

  dropMods
    :: (a -> Type)
    -> (a -> Type -> a)
    -> [a]
    -> Q [a]
  dropMods typ setType = mapM $ \col ->
    let
      go :: Type -> Q Type
      go t@(AppT modif a) = do
        isModInst <- reifyInstances ''IsModifier [modif]
        if null isModInst
          then return t
          else go a
      go a = return a
    in setType col `fmap` go (typ col)

  snd3 (n, s, t) = (n, setStrict s, t)

  procMods :: Con -> Q Con
  procMods con = case con of
    RecC name varStrictTypes -> RecC name <$> dropMods
      (\(_, _, t) -> t)
      (\(v,s,_) t -> (v,s,t))
      (map snd3 varStrictTypes)
    NormalC name strictTypes -> NormalC name <$> dropMods
      snd
      (\(a,_) t -> (a,t))
      (map (\(s, t) -> (setStrict s, t)) strictTypes)
    _ -> return con

tableQ :: Name -> [String] -> [Type] -> Q Type
tableQ _db names types = foldr
  (\(n,ty) b -> [t|Column $(litT (strTyLit n)) $(return ty) ': $b|])
  [t| '[] |]
  (zip names types)

columns :: Name -> Con -> Q Type
columns db con = case con of
  NormalC name strictTypes ->
    let types = map snd strictTypes
        names = take (length types) (genColumnNames name)
    in tableQ db names types
  RecC _name varStrictTypes ->
    let types = map (\(_,_,t) -> t)          varStrictTypes
        names = map (\(v,_,_) -> nameBase v) varStrictTypes
    in tableQ db names types
  _ -> fail ""

tables :: Name -> Q [Dec] -> Q [Dec]
tables backend getDataDecs = do
  dataDecs <- getDataDecs
  models   <- forM dataDecs $ \dec@(DataD _ typName _ cons _) ->
    liftM2 (:) (removeModifiers dec)
    [d|instance Model $(conT backend) $(conT typName) where
         type Columns $(conT typName) = $(columns backend (head cons))
         type PK      $(conT typName) = PkOf $(columns backend (head cons))
         schema _ = $(createTable backend dec)
         table  _ = buildTable
      |]
  return (concat models)

createTable' :: Name -> Name -> [String] -> [Type] -> Q Exp
createTable' backend hstype names types = do
  btypes    <- mapM (qbackendType     backend) types
  btypemods <- mapM (qbackendTypeMods backend) types
  quoteExp Hasql.stmt $
    "CREATE TABLE " ++ nameBase hstype
    ++ "( "
    ++ intercalate ", " (map column (zip3 names btypes btypemods))
    ++ ")"
 where
  column (name, dbtype, dbtypemod) =
    name ++ " " ++ dbtype ++ " " ++ dbtypemod

createTable :: Name -> Dec -> Q Exp
createTable backend (DataD _cxt _typ _tyvars constrs _derivs) =
  case constrs of
    [RecC name varStrictTypes] ->
        let (names, _, types) = unzip3 varStrictTypes
        in createTable' backend name (map nameBase names) types
    [NormalC name strictTypes] ->
        let types = map snd strictTypes
            names = take (length types) (genColumnNames name)
        in createTable' backend name names types
    _ -> fail $
            "Hasql.Typed.table " ++ show constrs

--------------------------------------------------------------------------------
-- template haskell utility

qbackendType :: Name -> Type -> Q String
qbackendType backend hstype' = do
  let
    getType t@(AppT (AppT (AppT (ConT ref') refTy) (LitT (StrTyLit fk))) ty)
      | ref' == ''References = qRunIO (print ("yeah",t)) >> getType ty
    getType t@(AppT (ConT constr) ty)
      | constr == ''PrimaryKey = qRunIO (print t) >> getType ty
      | constr == ''Unique     = qRunIO (print t) >> getType ty
      | constr == ''Maybe      = qRunIO (print t) >> getType ty
    getType t = do
      qRunIO (print t)
      return t

  hstype <- getType hstype'
  inst   <- reifyInstances ''DBType [ ConT backend, hstype ]
  qRunIO (print inst)
  case inst of
    [TySynInstD _ (TySynEqn _ (LitT (StrTyLit t)))] -> return t
    _                                               -> return ""


qbackendTypeMods :: Name -> Type -> Q String
qbackendTypeMods _backend hstype = do
  null' <- mod ''Maybe      $ \t -> if null t        then "NOT NULL"    else ""
  pk    <- mod ''PrimaryKey $ \t -> if length t == 1 then "PRIMARY KEY" else ""
  uniq  <- mod ''Unique     $ \t -> if length t == 1 then "UNIQUE"      else ""
  return (unwords [ if null pk
                    then null'
                    else ""
                  , pk
                  , if null pk
                    then uniq
                    else ""])
 where
  setMod f t  = f t
  mod    t f = fmap (setMod f) (reifyInstances ''Is [ ConT t, hstype ])

genColumnNames :: Name -> [String]
genColumnNames n = map (\i -> nameBase n ++ show i) [0..]
