{-# LANGUAGE GADTs, KindSignatures,DataKinds, ScopedTypeVariables, RankNTypes  #-}
module CoreTT where


import Numeric.Natural
import Data.String
import qualified Data.Vector as V ()
import qualified Data.Vector.Unboxed as UV
import Data.Vector (Vector)
import Data.Word
{-

this is going to be one giant mutually recursive data type,
cause dependent types


also lets ignore  source name strings for now :)

-}

data Symbol = Sym !(UV.Vector Char) -- this is basically utf32 :)
  deriving (Eq,Ord,Show)
instance IsString Symbol where
  fromString = \ x -> Sym $ UV.fromList x


data FullyQName = FQName !(Vector Symbol) !Symbol
  deriving (Eq,Ord,Show)
 --- namespace prefix then "unqualified name "
 -- for good error messages this will need improvements

--data Ty :: Type -> Type where
--- co-debruin style local names, "how many "
data LocalName =
      LName !Word64  -- how many binder groups up we need to go
            !Word64  -- which element of that binder group we reference , left->right, first -> last
  deriving (Eq,Ord,Show)

data Name  where
  Local :: !LocalName -> Name
  -- ^ debruijn levels / co-debruin indices ... counts how many binders up
  Global :: !FullyQName  -> Name
  deriving (Eq,Ord,Show)


data UsageVal  =
  Irrelevant    |
  ExactlyLinear |
  Unrestricted
  deriving(Eq,Ord,Show)
  -- we may want more points in the lattice, start with this
  -- Irrelevant is the most restrictive on where things can be consumed, Unrestricted is the most permissive

  --AtmostLinear :: Usage a
  --AtleastLinear :: Usage a

data DerivingNotes =
   DerEq
   | DerOrd
   | DerPrettyPrint
data Decl  where
  DecDataInd :: Symbol   -- name
     -> Term  -- kind sig for datatype
     ->  Vector (Symbol,Term ) -- constructor names and type signatures
     -> [DerivingNotes]
     -> Decl
  --- DecDataInd takes a sum (sequence) of constructor names paired with their typesigs  and some deriving directions

  --

data Literal  where
  LitNat :: !Natural -> Literal
  LitInteger :: !Integer -> Literal
  LitRational :: !Rational -> Literal
  LitDouble :: !Double -> Literal
  LitFloat :: !Float -> Literal
  LitUtfString :: !Symbol -> Literal

data Term  where
  Var :: Name -> Term
  TypeAnnotation :: Term -> Term  -> Term
  App :: Term -> Vector (Term ) -> Term
  Lit :: !Literal -> Term
  ---

  ---
  -- types
  PiSigma :: Vector (Maybe Symbol, UsageVal, Term ) -- domain/ pi telescope , maybe a name, always a usage and type
           ->Vector (Maybe Symbol, UsageVal, Term )
           -> Term
  -- kinds / sorts
  Univ :: Word64  -- universes have a level arg, here writing as a word64, cause why not
    -> Term




