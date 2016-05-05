{-# LANGUAGE TemplateHaskell, ViewPatterns #-}

module Servant.Record.TH
       ( indexedRecord
       , servantRecord
       , servantRecords
       ) where

import Control.Lens
import GHC.Generics
import Language.Haskell.TH

import Servant.Record

--
-- Generation of records with fields of types `TypeF n`, where `n :: Nat`
--

-- Less type safe that we want due to multi-stage compilation absense, so we
-- can't use things from the same module. If we could, we could take Proxy
-- as an argument; much nicer.
indexedRecord :: String -> Type -> [String] -> Q [Dec]
indexedRecord (mkName -> name) typ accs = do
  let nat 0 = PromotedT 'Z
      nat n = AppT (PromotedT 'S) (nat (n - 1))
  let accs' = map (\(x, n) -> (mkName x, IsStrict, AppT typ (nat n))) $ zip accs [0..]
  return [DataD [] name [] [RecC name accs'] []]

--
-- Generation of records for servant API and helper functions.
--

servantRecord :: String -> Name -> Name -> [String] -> Q [Dec]
servantRecord name api dest accs = do
  let typ = AppT (AppT (ConT ''NthT) (ConT dest)) (ConT api)
  [DataD _ name' _ cons _] <- indexedRecord name typ accs
  return [DataD [] name' [] cons [''Generic]]

servantRecords :: String -> Name -> [(Name, String)] -> [String] -> Q [Dec]
servantRecords name api dests accs =
  concat <$> mapM (\(dest, p) -> servantRecord (name ++ p) api dest (map (++p) accs)) dests
