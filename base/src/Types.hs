module Types where

import Data.Maybe (fromJust)

import Test.QuickCheck     (Arbitrary (..))
import Test.QuickCheck.Gen (choose, chooseAny)


{-@ assume abs :: _ -> {v:_ | 0 <= v} @-}

{-@ type Percentage = {v:Double | 0 <= v && 1 >= v} @-}
{-@ newtype Percentage = Percentage Percentage @-}
newtype Percentage = Percentage Double
                   deriving (Show)

instance Arbitrary Percentage where
  arbitrary = choose (0.0, 1.0) >>= \val -> if val >= 0.0 && val <= 1.0
                                             then return $ Percentage val
                                             else error "Should never happen"


percentage :: Double -> Maybe Percentage
percentage f | f >= 0 && f <= 1.0 = Just $ Percentage f
             | otherwise          = Nothing


percentage' :: Double -> Percentage
percentage' = fromJust . percentage


{-@ type PositiveDouble = {v:Double | 0 <= v} @-}
{-@ newtype PositiveDouble = PositiveDouble PositiveDouble @-}
newtype PositiveDouble = PositiveDouble Double
                       deriving (Show)

instance Arbitrary PositiveDouble where
  arbitrary = PositiveDouble . abs <$> chooseAny
