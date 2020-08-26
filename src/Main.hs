{-# LANGUAGE FlexibleContexts #-}

module Main where

import           System.Environment             ( getArgs )

import           Graphics.Image                 ( Array
                                                , Image
                                                , Pixel(..)
                                                , RGBA
                                                , VU(..)
                                                , makeImage
                                                , readImageRGB
                                                , writeImage
                                                )

collage
  :: Array arr RGBA e => [Image arr cs e] -> (Int, Int) -> Image arr RGBA e
collage images (x, y) = makeImage (x, y) (\_ -> PixelRGBA 0 0 0 0)

main :: IO ()
main = do
  args <- getArgs
  let nArgs      = length args
      x          = read $ head args
      y          = read $ head $ drop 1 args
      imagePaths = take (nArgs - 2 - 1) $ drop 2 args
      outputPath = last args

  images <- sequence $ map (readImageRGB VU) imagePaths
  writeImage outputPath $ collage images (x, y)
