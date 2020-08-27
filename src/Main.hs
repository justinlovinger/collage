{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Data.Bifunctor                 ( first )
import           System.Environment             ( getArgs )
import           System.Random                  ( RandomGen
                                                , getStdGen
                                                , randomR
                                                , split
                                                )
import           Graphics.Image                 ( Array
                                                , Bilinear(..)
                                                , Border(..)
                                                , Image
                                                , Pixel(..)
                                                , RGB
                                                , VS(..)
                                                , dims
                                                , makeImage
                                                , readImageRGB
                                                , resize
                                                , superimpose
                                                , writeImage
                                                )

data Position = UpLeft | DownRight

collage
  :: RandomGen g
  => [FilePath]
  -> (Int, Int)
  -> (Int, Int)
  -> g
  -> IO (Image VS RGB Double)
collage imagePaths (wt, ht) (w, h) g
  | w <= wt || h <= ht = pure $ blank (w, h)
  | otherwise = do
    let ((ga, gb), gc) = first split $ split g

    -- Select a random image
    -- and fill the remaining space
    -- with a collage of random images.
    imageA <- (pure . fit (w, h)) =<< (readImageRGB VS $ choose imagePaths ga)
    imageB <- collage imagePaths (wt, ht) (remaining (w, h) imageA) gb

    pure $ case choose [UpLeft, DownRight] gc of
      UpLeft ->
        combine (w, h) (0, 0) imageA (posDownRight (w, h) imageB) imageB
      DownRight ->
        combine (w, h) (posDownRight (w, h) imageA) imageA (0, 0) imageB
 where
  -- If `image` takes width of canvas,
  -- a horizontal slice remains.
  -- Otherwise, a vertical slice remains.
  remaining (w, h) image = if w' == w then (w, h - h') else (w - w', h)
    where (h', w') = dims image

  posDownRight (w, h) image = (w - w', h - h') where (h', w') = dims image

  combine (w, h) (ax, ay) imageA (bx, by) imageB =
    superimpose (by, bx) imageB $ superimpose (ay, ax) imageA $ blank (w, h)

blank :: Array arr RGB e => (Int, Int) -> Image arr RGB e
blank (w, h) = makeImage (h, w) (\_ -> PixelRGB 0 0 0)

choose :: RandomGen g => [a] -> g -> a
choose xs g = xs !! (fst $ randomR (0, length xs - 1) g)

fit :: Array arr cs e => (Int, Int) -> Image arr cs e -> Image arr cs e
fit (w, h) image = resize Bilinear Edge (nh, nw) image where
  (ih, iw) = dims image
  rw       = fromIntegral iw / fromIntegral w
  rh       = fromIntegral ih / fromIntegral h
  (nw, nh) = if rw > rh
    then (w, round $ fromIntegral ih / rw)
    else (round $ fromIntegral iw / rh, h)

main :: IO ()
main = do
  args <- getArgs
  let nArgs            = length args
      w                = read $ head args
      h                = read $ head $ drop 1 args
      imagePaths       = take (nArgs - 2 - 1) $ drop 2 args
      outputPath       = last args

      thresholdPercent = 0.05
      threshold'       = threshold thresholdPercent

  g        <- getStdGen
  outImage <- collage imagePaths (threshold' w, threshold' h) (w, h) g
  writeImage outputPath outImage
 where
  threshold :: RealFrac a => Integral b => a -> b -> b
  threshold tp x = ceiling $ tp * fromIntegral x
