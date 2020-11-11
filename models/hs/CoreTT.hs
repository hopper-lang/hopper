{-# LANGUAGE GADTs, KindSignatures,DataKinds, ScopedTypeVariables, RankNTypes  #-}
module CoreTT where


import qualified Data.Vector as V ()
import Data.Vector (Vector)
import Data.Word
{-

this is going to be one giant mutually recursive data type,
cause dependent types


also lets ignore  source name strings for now :)

-}


--data Ty :: Type -> Type where
--- co-debruin style local names, "how many "
data LocalName =
      LName !Word64  -- how many binder groups up we need to go
            !Word64  -- which element of that binder group we reference , left->right, first -> last
  deriving (Eq,Ord,Show)

data Name  where
  Local :: LocalName -> Name
  -- ^ debruijn levels / co-debruin indices ... counts how many binders up
  Global :: Vector String -> Name
  deriving (Eq,Ord,Show)


data UsageVal  =
  Irrelevant |
  Unrestricted |
  ExactlyLinear
  -- we may want more points in the lattice, start with this
  --AtmostLinear :: Usage a
  --AtleastLinear :: Usage a

data DerivingNotes =
   DerEq
   | DerOrd
   | DerPrettyPrint
data Decl  where
  DecDataInd :: String ->  Vector (String,Term ) -> [DerivingNotes] -> Decl
  --- DecDataInd takes a sum (sequence) of constructor names paired with their typesigs  and some deriving directions

  --

data Term  where
  Var :: Name -> Term
  TypeAnnotation :: Term -> Term  -> Term
  App :: Term -> Vector (Term ) -> Term
  ---
  -- types


  -- kinds




