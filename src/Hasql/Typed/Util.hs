{-# LANGUAGE ConstraintKinds, DataKinds, GADTs, MultiParamTypeClasses,
             TypeFamilies, TypeOperators #-}
module Hasql.Typed.Util where
import GHC.Exts     (Constraint)
import GHC.TypeLits

class Yes
instance Yes

class No (error :: Symbol)
