{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Data.Bifunctor                 ( first )
import           System.Environment             ( getArgs )
import           System.Random                  ( RandomGen
                                                , getStdGen
                                                , randomR
                                                )
import           Graphics.Image                 ( Array
                                                , Bilinear(..)
                                                , Border(..)
                                                , Image
                                                , Pixel(..)
                                                , RGBA
                                                , VU(..)
                                                , dims
                                                , makeImage
                                                , readImageRGBA
                                                , resize
                                                , superimpose
                                                , writeImage
                                                )

data Position = UpLeft | DownRight

collage
  :: Array arr RGBA e
  => RandomGen g => [Image arr RGBA e] -> (Int, Int) -> g -> Image arr RGBA e
collage _      (0, 0) _ = blank (0, 0)
-- Select a random image
-- and fill the remaining space
-- with a collage of random images.
collage images (w, h) g = superimpose (oy, ox)
                                      (collage images (w', h') g'')
                                      base where
  (image   , g'      ) = first (fit (w, h)) $ choose images g
  (ih      , iw      ) = dims image
  (pos     , g''     ) = choose [UpLeft, DownRight] g'
  ((ix, iy), (ox, oy)) = case pos of
    -- If chosen image takes width of canvas,
    -- shift remaining images down.
    -- Otherwise,
    -- shift remaining images right.
    UpLeft    -> ((0, 0), if iw == w then (0, ih) else (iw, 0))
    DownRight -> ((h - ih, w - iw), (0, 0))
  base     = superimpose (ix, iy) image (blank (w, h))
  -- If chosen image takes width of canvas,
  -- then we have a horizontal slice to work with.
  -- Otherwise,
  -- we have a vertical slice.
  (w', h') = if iw == w then (w, h - ih) else (w - iw, h)

blank :: Array arr RGBA e => (Int, Int) -> Image arr RGBA e
blank (w, h) = makeImage (h, w) (\_ -> PixelRGBA 0 0 0 0)

choose :: RandomGen g => [a] -> g -> (a, g)
choose xs g = (xs !! i, g') where (i, g') = randomR (0, length xs - 1) g

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
  let nArgs      = length args
      w          = read $ head args
      h          = read $ head $ drop 1 args
      imagePaths = take (nArgs - 2 - 1) $ drop 2 args
      outputPath = last args

  images <- sequence $ map (readImageRGBA VU) imagePaths
  g      <- getStdGen
  writeImage outputPath $ collage images (w, h) g
