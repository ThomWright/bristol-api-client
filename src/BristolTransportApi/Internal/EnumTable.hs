{-# LANGUAGE RankNTypes #-}

module BristolTransportApi.Internal.EnumTable
( createTable
) where

import Data.Data

-- From this: http://stackoverflow.com/a/28789644
-- Creates a table for an enum mapping which produces a compile warning when non-exhaustive (if -Wall is enabled)
createTable :: Data a => (a -> Int) -> a -> [(a, Int)]
createTable convert dataConst = map (toTuple convert) $ dataTypeConstrs $ dataTypeOf dataConst

toTuple :: Data a => (a -> Int) -> Constr -> (a, Int)
toTuple convert dataConstructor = (c, convert c)
  where
    c = fromConstr dataConstructor :: forall a. Data a => a
