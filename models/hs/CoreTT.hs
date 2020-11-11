{-# LANGUAGE GADTs, KindSignatures,DataKinds, ScopedTypeVariables, RankNTypes  #-}
module CoreTT where

import GHC.Types

{-



-}


data Term :: Type -> Type where
  Var :: a -> Term a
