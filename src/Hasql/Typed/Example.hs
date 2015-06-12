{-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE DataKinds, QuasiQuotes, TemplateHaskell, TypeFamilies,
             TypeOperators #-}
module Hasql.Typed.Example where
import Control.Lens
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

models ''Test [d|
  data Tag = Tag
    { tagId   :: {-# UNPACK #-} !(PrimaryKey Int)
    , tagName :: !(Unique String)
    } deriving (Show)

  data Post = Post
    { postId      :: {-# UNPACK #-} !(PrimaryKey Int)
    , postTag     :: {-# UNPACK #-} !((Tag    ^. "tagId")      Int)
    , postAuthor  :: {-# UNPACK #-} !((Author ^. "postAuthor") Int)
    , postContent :: !String
    } deriving (Show)

  data Author = Author
    { authorId    :: {-# UNPACK #-} !(PrimaryKey Int)
    , authorName  :: !String
    , authorEmail :: !String
    } deriving (Show)
  |]

{-
>>> :i Tag
data Tag = Tag {tagId :: !Int, tagName :: !String}
        -- Defined at src/Hasql/Typed/Example.hs:22:1
instance Show Tag -- Defined at src/Hasql/Typed/Example.hs:22:1
instance Model Test Tag
  -- Defined at src/Hasql/Typed/Example.hs:22:1
type instance Columns Tag
  = '[Column "tagId" (PrimaryKey Int),
      Column "tagName" (Unique String)]
        -- Defined at src/Hasql/Typed/Example.hs:22:1
type instance PrimaryKeys Tag = '[Column "tagId" Int]
        -- Defined at src/Hasql/Typed/Example.hs:22:1
type instance Uniques Tag = '[Column "tagName" String]
        -- Defined at src/Hasql/Typed/Example.hs:22:1
type instance References Tag = '[]
        -- Defined at src/Hasql/Typed/Example.hs:22:1
type instance Schema Tag
  = "CREATE TABLE \"Tag\"(\"tagId\" int8  PRIMARY KEY, \"tagName\" text  UNIQUE)"
        -- Defined at src/Hasql/Typed/Example.hs:22:1
type instance TableName Tag = "Tag"
        -- Defined at src/Hasql/Typed/Example.hs:22:1
>>> :i Post
data Post
  = Post {postId :: !Int,
          postTag :: !Int,
          postAuthor :: !Int,
          postContent :: !String}
        -- Defined at src/Hasql/Typed/Example.hs:22:1
instance Show Post -- Defined at src/Hasql/Typed/Example.hs:22:1
instance Model Test Post
  -- Defined at src/Hasql/Typed/Example.hs:22:1
type instance Columns Post
  = '[Column "postId" (PrimaryKey Int),
      Column "postTag" ((^.) Tag "tagId" Int),
      Column "postAuthor" ((^.) Author "postAuthor" Int),
      Column "postContent" String]
        -- Defined at src/Hasql/Typed/Example.hs:22:1
type instance PrimaryKeys Post = '[Column "postId" Int]
        -- Defined at src/Hasql/Typed/Example.hs:22:1
type instance Uniques Post = '[]
        -- Defined at src/Hasql/Typed/Example.hs:22:1
type instance References Post
  = '[Column "postAuthor" ((^.) Author "postAuthor" Int),
      Column "postTag" ((^.) Tag "tagId" Int)]
        -- Defined at src/Hasql/Typed/Example.hs:22:1
type instance Schema Post
  = "CREATE TABLE \"Post\"(\"postId\" int8  PRIMARY KEY, \"postTag\" int8  REFERENCES \"Tag\"(\"tagId\"), \"postAuthor\" int8  REFERENCES \"Author\"(\"postAuthor\"), \"postContent\" text )"
        -- Defined at src/Hasql/Typed/Example.hs:22:1
type instance TableName Post = "Post"
        -- Defined at src/Hasql/Typed/Example.hs:22:1
-}
