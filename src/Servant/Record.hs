{-# LANGUAGE TypeFamilies, StandaloneDeriving, TypeOperators,
             TypeSynonymInstances, FlexibleInstances, FlexibleContexts,
             DataKinds, UndecidableInstances, PolyKinds #-}

module Servant.Record
       (recordIso
       , Nth
       , NthT
       ) where

import GHC.Generics hiding (from, to)
import GHC.TypeLits
import Control.Lens
import GHC.Generics.Lens

import Servant.API

--
-- Isomorphism beyween records and type-level "cons"es (:<|>)
--

recordIso :: (Generic a, Rep a ~ D1 dm (C1 cm fields), SumIso fields)
          => Iso' a (SumType fields)
recordIso = generic . _M1 . _M1 . sumIso

class SumIso (a :: k -> *) where
  type SumType a
  sumIso :: Iso' (a p) (SumType a)

instance SumIso (M1 i sm (Rec0 a)) where
  type SumType (M1 i sm (Rec0 a)) = a
  sumIso = _M1 . _K1

instance (SumIso a, SumIso b) => SumIso (a :*: b) where
  type SumType (a :*: b) = SumType a :<|> SumType b
  sumIso = iso (\(a :*: b) -> (a^.sumIso) :<|> (b^.sumIso))
               (\(a :<|> b) -> (a^.from sumIso) :*: (b^.from sumIso))

--
-- (:<|>)'s indexing
--

type family Nth (a :: k) (n :: Nat) :: k' where
  Nth (h :<|> t) 0 = h
  Nth h 0 = h
  Nth (h :<|> t) n = Nth t (n - 1)

--
-- Helpful synonym
--

type NthT (t :: k -> *) a n = t (Nth a n)
