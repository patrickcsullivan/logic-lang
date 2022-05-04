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
  deriving (Eq, Ord)

instance Show FnConst where
  show (FnConst name arity) = name ++ "/" ++ show arity

newtype ObjConst = ObjConst
  { objConstName :: String
  }
  deriving (Eq, Ord)

instance Show ObjConst where
  show (ObjConst name) = name
