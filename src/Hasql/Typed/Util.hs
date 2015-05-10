{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, FlexibleInstances,
             GADTs, MultiParamTypeClasses, PolyKinds, TypeFamilies,
             TypeOperators #-}
module Hasql.Typed.Util where
import GHC.TypeLits

class Yes
instance Yes

class No (error :: Symbol)

class Is f a
instance Is f (f a)

