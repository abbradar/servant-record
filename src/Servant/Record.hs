{-# LANGUAGE TypeFamilies, StandaloneDeriving, TypeOperators,
             TypeSynonymInstances, FlexibleInstances, FlexibleContexts,
             DataKinds, UndecidableInstances, PolyKinds, MultiParamTypeClasses,
             ScopedTypeVariables, ConstraintKinds #-}

module Servant.Record
       {-( recordAlts
       , Nat(..)
       , Nth
       , NthT
       , toAlts
       , fromAlts
       )-} where

import Data.Proxy
import GHC.Generics hiding (from, to)
import Control.Lens
import GHC.Generics.Lens
import Control.Monad.Reader
import Unsafe.Coerce

import Servant.API

--
-- Utilities
--

data Nat = Z | S !Nat

type family Add (a :: Nat) (b :: Nat) where
  Add Z b = b
  Add ('S a) b = 'S (Add a b)

type family AltsSize t :: Nat where
  AltsSize (h :<|> t) = 'S (AltsSize t)
  AltsSize t = 'S Z

type family TreeSize t :: Nat where
  TreeSize (a :*: b) = Add (TreeSize a) (TreeSize b)
  TreeSize (M1 i sm (Rec0 a)) = 'S Z

class AppendAlts' (n :: Nat) a b where
  type AppendAltsT' n a b
  appendAlts' :: Proxy n -> a -> b -> AppendAltsT' n a b

instance AppendAlts' ('S Z) at bt where
  type AppendAltsT' ('S Z) at bt = at :<|> bt
  appendAlts' _ a b = a :<|> b

instance forall n ht tt bt. (AppendAlts' ('S n) tt bt) => AppendAlts' ('S ('S n)) (ht :<|> tt) bt where
  type AppendAltsT' ('S ('S n)) (ht :<|> tt) bt = ht :<|> AppendAltsT' ('S n) tt bt
  appendAlts' _ (h :<|> t) b = h :<|> appendAlts' (Proxy :: Proxy ('S n)) t b

type AppendAlts a b = AppendAlts' (AltsSize a) a b
type AppendAltsT a b = AppendAltsT' (AltsSize a) a b

appendAlts :: forall a b. AppendAlts a b => a -> b -> AppendAltsT a b
appendAlts a b = appendAlts' (Proxy :: Proxy (AltsSize a)) a b

class SplitAlts (n :: Nat) alts where
  type SplitAltsLeft n alts
  type SplitAltsRight n alts
  splitAlts :: Proxy n -> alts -> (SplitAltsLeft n alts, SplitAltsRight n alts)

instance SplitAlts ('S Z) (ht :<|> tt) where
  type SplitAltsLeft ('S Z) (ht :<|> tt) = ht
  type SplitAltsRight ('S Z) (ht :<|> tt) = tt
  splitAlts _ (h :<|> t) = (h, t)

instance forall n ht tt. (SplitAlts ('S n) tt) => SplitAlts ('S ('S n)) (ht :<|> tt) where
  type SplitAltsLeft ('S ('S n)) (ht :<|> tt) = ht :<|> SplitAltsLeft ('S n) tt
  type SplitAltsRight ('S ('S n)) (ht :<|> tt) = SplitAltsRight ('S n) tt
  splitAlts _ (h :<|> t) = (h :<|> left, right)
    where (left, right) = splitAlts (Proxy :: Proxy ('S n)) t

--
-- Isomorphism beyween records and chains of alternatives (:<|>)
--

recordAlts :: (Generic a, Rep a ~ D1 dm (C1 cm fields), SumIso fields)
           => Iso' a (SumType fields)
recordAlts = generic . _M1 . _M1 . sumIso

class SumIso (a :: k -> *) where
  type SumType a
  sumIso :: Iso' (a p) (SumType a)

instance SumIso (M1 i sm (Rec0 a)) where
  type SumType (M1 i sm (Rec0 a)) = a
  sumIso = _M1 . _K1

-- TODO: Requires proof
instance forall a n1 b n2.
         ( SumIso a
         , SumIso b
         , TreeSize a ~ n1
         , SplitAlts n1 (AppendAltsT (SumType a) (SumType b))
         , AppendAlts (SumType a) (SumType b)
         ) => SumIso (a :*: b) where
  type SumType (a :*: b) = AppendAltsT (SumType a) (SumType b)
  sumIso = iso (\(a :*: b) -> appendAlts (a^.sumIso) (b^.sumIso))
               (\alts -> let (l, r) = unsafeCoerce $ splitAlts (Proxy :: Proxy n1) alts
                        in l^.from sumIso :*: r^.from sumIso
               )

--
-- (:<|>)'s indexing
--

type family Nth (a :: k) (n :: Nat) :: k' where
  Nth (h :<|> t) Z = h
  Nth h Z = h
  Nth (h :<|> t) ('S n) = Nth t n

--
-- Helpful synonym
--

type NthT (t :: k -> *) a n = t (Nth a n)

--
-- Convenience functions
--

toAlts :: (Generic a, Rep a ~ D1 dm (C1 cm fields), SumIso fields)
       => a -> SumType fields
toAlts = view recordAlts

-- FIXME: workaround
fromAlts :: (Generic a, Rep a ~ D1 dm (C1 cm fields), SumIso fields,
            f ~ SumType fields)
         => f -> a
fromAlts = view (from recordAlts)
