{-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE DataKinds, QuasiQuotes, TemplateHaskell, TypeOperators #-}
module Hasql.Typed.Example where
import Hasql.Typed


tables ''Test [d|
  data Tag = Tag
    { tagId   :: {-# UNPACK #-} !(PrimaryKey Int)
    , tagName :: !(Unique String)
    }

  data Author = Author
    { authorId    :: !(PrimaryKey Int)
    , authorName  :: !(Maybe String)
    , authorEmail :: !(Unique String)
    }

  data Post = Post
    { postAuthor  :: (Author `References` "authorId") Int
    }
  |]


{-
>>> :r
[3 of 3] Compiling Hasql.Typed.Example       ( Hasql/Typed/Example.hs, interpreted )
Hasql/Typed/Example.hs:(5,1)-(16,3): Splicing declarations
    tables
      ''Test
      [d| data Tag_aMYh
            = Tag_aMYi {tagId_aMYj :: {-# UNPACK #-} !(PrimaryKey Int),
                        tagName_aMYk :: Unique String}
            deriving (Show, Eq, Ord)
          data Author_aMYc
            = Author_aMYd {authorId_aMYe :: {-# UNPACK #-} !(PrimaryKey Int),
                           authorEmail_aMYf :: Unique String,
                           authorName_aMYg :: Maybe String}
            deriving (Show, Eq, Ord) |]
  ======>
    data Tag_aN0O
      = Tag_aN0P {tagId_aN0Q :: {-# UNPACK #-} !Int,
                  tagName_aN0R :: !String}
      deriving (Show, Eq, Ord)
    instance Model Test Tag_aN0O where
      schema _
        = \ -> Hasql.Backend.Stmt
                 "CREATE TABLE Tag( tagId int4 PRIMARY KEY , tagName text NOT NULL UNIQUE)"
                 Data.Vector.empty
                 True
    data Author_aN0S
      = Author_aN0T {authorId_aN0U :: {-# UNPACK #-} !Int,
                     authorEmail_aN0V :: !String,
                     authorName_aN0W :: !(Maybe String)}
      deriving (Show, Eq, Ord)
    instance Model Test Author_aN0S where
      schema _
        = \ -> Hasql.Backend.Stmt
                 "CREATE TABLE Author( authorId int4 PRIMARY KEY , authorEmail text NOT NULL UNIQUE, authorName text )"
                 Data.Vector.empty
                 True
Ok, modules loaded: Hasql.Typed, Hasql.Typed.Util, Hasql.Typed.Example.
>>>
-}
