module Syntax.Constant
  ( RltnConst (..),
    FnConst (..),
    ObjConst (..),
  )
where

data RltnConst = RltnConst
  { rltnConstName :: String,
    rltnConstArity :: Int
  }
  deriving (Eq, Ord, Show)

data FnConst = FnConst
  { fnConstName :: String,
    fnConstArity :: Int
  }
  deriving (Eq, Ord, Show)

newtype ObjConst = ObjConst
  { objConstName :: String
  }
  deriving (Eq, Ord, Show)
