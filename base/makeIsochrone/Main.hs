{-# LANGUAGE TypeApplications #-}
module Main where

import Conduit

import Control.Monad (when)

import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.Conduit.Lzma
import Data.Either (isRight)
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as V

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

import System.Environment

import MainSequenceModel

data MSModel = OldDSED | NewDSED | Yale2018 | PARSEC
             deriving (Read, Show)

class HasModelPath a where
  modelPath :: a -> FilePath -> FilePath

instance HasModelPath MSModel where
  modelPath OldDSED  base = base ++ "mainSequence/dsed_old.model.xz"
  modelPath NewDSED  base = base ++ "mainSequence/dsed_new.model.xz"
  modelPath PARSEC   base = base ++ "mainSequence/PARSEC.model.xz"
  modelPath Yale2018 base = base ++ "mainSequence/yale_2018.model.xz"

main :: IO ()
main = do
  cliArgs <- read @MSModel . head <$> getArgs

  runConduitRes ( sourceFile (modelPath cliArgs "models/")
                .| decompress Nothing
                .| lexModel
                .| parseModel
                .| mapM_C (liftIO . toFeHPlot))

toFeHPlot t = do
  let ((feh, y), ages) = t
      go (Age a _ _ fs) =
        let b = fs !! 1
            v = fs !! 2
            bmv = V.zipWith (-) b v
        in plot $ do
          pts <- points (show a) $ V.toList $ V.zip bmv v
          return $ pts & plot_points_style . point_radius .~ 1

  toFile (def & (fo_size .~ (7680, 5000)) -- (1920, 4000)) --1440))
              & (fo_format .~ PNG)
         ) ("/tmp/feh" ++ (if feh < 0 then 'm' else 'p'):show (abs feh) ++ "_y" ++ show y ++ ".png") $ do
    setColors (map opaque [blue, green, purple, yellow, red])

    layout_title .= "New DSED, [Fe/H] = " ++ show feh ++ ", Y = " ++ show y
    layout_x_axis .= (def & laxis_title .~ "B - V")
--                          & laxis_generate .~ scaledAxis def (-0.5, 2))
    layout_y_axis .= (def & laxis_reverse .~ True & laxis_title .~ "V"
                          & laxis_generate .~ scaledAxis def (-8, 15))
    mapM_ go $ S.elems ages
