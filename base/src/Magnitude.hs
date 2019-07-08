{-# LANGUAGE TemplateHaskell, TypeFamilies, MultiParamTypeClasses #-}
module Magnitude (AbsoluteMagnitude(..)) where

import Types.Internal
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed.Deriving

newtype AbsoluteMagnitude = MkAbsoluteMagnitude { unAbsoluteMagnitude :: Log10 }
        deriving (Show, Eq, Ord)

derivingUnbox "AbsoluteMagnitude"
  [t| AbsoluteMagnitude -> Log10 |]
  [| unAbsoluteMagnitude |]
  [| MkAbsoluteMagnitude |]
