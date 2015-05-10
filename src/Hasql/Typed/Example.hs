{-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE DataKinds, QuasiQuotes, TemplateHaskell, TypeFamilies,
             TypeOperators #-}
module Hasql.Typed.Example where
import Data.Int
import Data.Word
import Hasql.Typed

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

model ''Test . head =<< [d|
  data Tag = Tag
    { tagId   :: {-# UNPACK #-} !(PrimaryKey Int)
    , tagName :: !(Unique String)
    }
  |]

model ''Test . head =<< [d|
  data Post = Post
    { postId      :: {-# UNPACK #-} !(PrimaryKey Int)
    , postTag     :: {-# UNPACK #-} !((Tag `Reference` "tagId") Int)
    , postContent :: !String
    }
  |]

{-
>>> :i Tag
data Tag = Tag {tagId :: !Int, tagName :: !String}
instance Model Test Tag
type instance Columns Tag
  = '[Column "tagId" (PrimaryKey Int),
      Column "tagName" (Unique String)]
type instance PrimaryKeys Tag = '[Column "tagId" Int]
type instance Uniques Tag = '[Column "tagName" String]
type instance References Tag = '[]
type instance Schema Tag
  = "CREATE TABLE (\"tagId\" int8  PRIMARY KEY, \"tagName\" text  UNIQUE)"

>>> :i Post
data Post
  = Post {postId :: !Int, postTag :: !Int, postContent :: !String}
instance Model Test Post
type instance Columns Post
  = '[Column "postId" (PrimaryKey Int),
      Column "postTag" (Reference Tag "tagId" Int),
      Column "postContent" String]
type instance PrimaryKeys Post = '[Column "postId" Int]
type instance Uniques Post = '[]
type instance References Post
  = '[Column "postTag" (Reference Tag "tagId" Int)]
type instance Schema Post
  = "CREATE TABLE (\"postId\" int8  PRIMARY KEY, \"postTag\" int8  REFERENCES \"Tag\"(\"tagId\"), \"postContent\" text )"
-}
