{-# LANGUAGE DeriveGeneric, OverloadedStrings                   #-}
{-# LANGUAGE GADTs, QuasiQuotes, ViewPatterns, FlexibleContexts #-}

{-|

Abalone data set

UCI ML Repository link <https://archive.ics.uci.edu/ml/datasets/abalone>

-}

module Numeric.Datasets.Coal ( Coal, coal, date ) where

import qualified H.Prelude as H
import Language.R.QQ

import qualified Foreign.R as R

import Language.R.HExp ( hexp )
import Language.R.HExp ( HExp(..) )

import qualified Data.Vector.SEXP as V

import Numeric.Datasets

import Data.Csv
import GHC.Generics

data Coal = Coal
  { date :: Double
  } deriving (Show, Read, Generic)

instance FromRecord Coal

coal_r :: IO [Double]
coal_r = H.runRegion $ do
  [r| library('boot') |]
  (hexp -> Real v) <- R.cast R.SReal <$>
                      [r| coal$date |]
  return $ V.toList v

coal :: Dataset Coal
coal = const ((fmap . fmap) Coal coal_r)
