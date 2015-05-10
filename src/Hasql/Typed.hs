{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs,
             MultiParamTypeClasses, PolyKinds, ScopedTypeVariables,
             StandaloneDeriving, TemplateHaskell, TypeFamilies, TypeOperators,
             UndecidableInstances #-}
module Hasql.Typed where
import Control.Lens
import Control.Monad.State.Strict
--------------------------------------------------------------------------------
import Data.List
import Data.Proxy
--------------------------------------------------------------------------------
import GHC.Exts                   (Constraint)
import GHC.TypeLits
import Language.Haskell.TH        as TH
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

backendType :: KnownSymbol (DBType db hstype) => db -> hstype -> String
backendType (_db :: db) (_hstype :: hstype) =
  symbolVal (Proxy :: Proxy (DBType db hstype))

data Table db a where
  Empty  :: Table db '[]
  AddCol :: (name `NotElem` columns)
      => Column name hstype
      -> Table db columns
      -> Table db (Column name hstype ': columns)

deriving instance Show (Table db a)

class Model db a where
  type Columns     a :: [*]
  type PrimaryKeys a :: [*]
  type Uniques     a :: [*]
  type References  a :: [*]
  type Schema      a :: Symbol
  table  :: Table db (Columns a)
  schema :: proxy a -> Hasql.Stmt db

class BuildTable db a where
  buildTable  :: Table db a

instance (name `NotElem` columns, BuildTable db columns) =>
         BuildTable db (Column name hstype ': columns) where
  {-# INLINE buildTable #-}
  buildTable = AddCol Column buildTable

instance BuildTable db '[] where
  {-# INLINE buildTable #-}
  buildTable = Empty

newtype Reference parent column a = Reference a
newtype PrimaryKey a = PrimaryKey a
newtype Unique a = Unique a

class IsModifier (a :: * -> *)
instance IsModifier (Reference parent column)
instance IsModifier PrimaryKey
instance IsModifier Unique

data ColumnsInfo = ColumnsInfo
  { _primaries  :: [VarStrictType]
  , _uniques    :: [VarStrictType]
  , _references :: [VarStrictType]
  }
makeLenses ''ColumnsInfo

identifier :: Name -> String
identifier = show . nameBase

dropTypeMods :: VarStrictType -> State ColumnsInfo (String, VarStrictType)
dropTypeMods (v,s,t0) =
  let
    dropper :: Type -> State ColumnsInfo (String, Type)
    dropper ty = case ty of
      AppT (ConT c) t
        | c == ''PrimaryKey -> do
            (sql, t') <- dropper t
            primaries %= (:) (v,s,t')
            return (sql ++ " PRIMARY KEY", t')

        | c == ''Unique -> do
            (sql, t') <- dropper t
            uniques %= (:) (v,s,t')
            return (sql ++ " UNIQUE", t')

      AppT (AppT (AppT (ConT c) (ConT parent)) (LitT (StrTyLit column)) ) t
        | c == ''Reference -> do
            (sql, t') <- dropper t
            references %= (:)
              (v, s,
               AppT
               (AppT (AppT (ConT c) (ConT parent)) (LitT (StrTyLit column))) t'
              )
            return (sql
                    ++ " REFERENCES "
                    ++ identifier parent
                    ++ "(" ++ show column ++")", t')

      _ -> return ("", ty)

  in do
    (mods, t') <- dropper t0
    return (mods, (v,s,t'))

simpleDropTypeMods :: Type -> Type
simpleDropTypeMods ty = case ty of
  AppT (ConT c) t
    | c == ''PrimaryKey -> simpleDropTypeMods t
    | c == ''Unique     -> simpleDropTypeMods t
  AppT (AppT (AppT (ConT c) (ConT _)) (LitT (StrTyLit _)) ) t
    | c == ''Reference  -> simpleDropTypeMods t
  _ -> ty

columns :: Name -> [VarStrictType] -> Q Type
columns _ =
  foldr
  (\(v,_,t) acc ->
    [t|Column $(litT (strTyLit (nameBase v))) $(pure t) ': $acc |])
  [t| '[] |]

formatSchema :: Name -> [(String, VarStrictType)] -> Q String
formatSchema db vsts = do
  cols <- mapM format vsts
  return $!
    "CREATE TABLE "
    ++ "("
    ++ intercalate ", " cols
    ++ ")"
 where
  format (mods, (v, _, ty)) = do
    qRunIO (print (simpleDropTypeMods ty, db))
    insts <- reifyInstances ''DBType [ConT db, simpleDropTypeMods ty]
    qRunIO (print insts)

    let [TySynInstD _ (TySynEqn _ (LitT (StrTyLit s))) ] = insts
    return $! identifier v ++ " " ++ s ++ " " ++ mods

model :: Name -> Dec -> Q [Dec]
model backend dec = do
  let
    DataD cxt' name tyvars [RecC constr v's'types] derivs = dec

    (typesNoMods, ColumnsInfo prims uniqs refs) =
      runState (mapM dropTypeMods v's'types) (ColumnsInfo [] [] [])

    decNoMods = DataD cxt' name tyvars [RecC constr (map snd typesNoMods)] derivs

  qRunIO (print prims)
  qRunIO (print uniqs)

  sql <- formatSchema backend typesNoMods

  (decNoMods :) <$>
    [d|instance Model $(conT backend) $(conT name) where
         type Columns     $(conT name) = $(columns backend v's'types)
         type PrimaryKeys $(conT name) = $(columns backend prims)
         type Uniques     $(conT name) = $(columns backend uniqs)
         type References  $(conT name) = $(columns backend refs)
         type Schema      $(conT name) = $(litT (strTyLit sql))

         table = buildTable
         schema _ = $(quoteExp Hasql.stmt sql)
      |]
