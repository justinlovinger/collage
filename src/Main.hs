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
import           Options.Applicative            ( Parser
                                                , ReadM
                                                , (<**>)
                                                , argument
                                                , eitherReader
                                                , execParser
                                                , fullDesc
                                                , header
                                                , help
                                                , helper
                                                , info
                                                , long
                                                , metavar
                                                , option
                                                , progDesc
                                                , short
                                                , some
                                                , str
                                                )

data Position = UpLeft | DownRight

collage
  :: RandomGen g
  => Pixel RGB Double
  -> [FilePath]
  -> (Int, Int)
  -> (Int, Int)
  -> g
  -> IO (Image VS RGB Double)
collage fillPx imagePaths (wt, ht) (w, h) g
  | w <= wt || h <= ht = pure $ fill' (w, h)
  | otherwise = do
    let ((ga, gb), gc) = first split $ split g

    -- Select a random image
    -- and fill the remaining space
    -- with a collage of random images.
    imageA <- (pure . fit (w, h)) =<< (readImageRGB VS $ choose imagePaths ga)
    imageB <- collage fillPx imagePaths (wt, ht) (remaining (w, h) imageA) gb

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
    superimpose (by, bx) imageB $ superimpose (ay, ax) imageA $ fill' (w, h)

  fill' = fill fillPx

fill :: Array arr cs e => Pixel cs e -> (Int, Int) -> Image arr cs e
fill px (w, h) = makeImage (h, w) (\_ -> px)

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

data Args = Args
  { imagePaths :: [FilePath]
  , outputPath :: FilePath
  , width :: Int
  , height :: Int
  }

main :: IO ()
main = do
  args <- execParser $ info
    (argsParser <**> helper)
    (fullDesc <> header "collage - create an image collage" <> progDesc
      "Create a collage of images randomly chosen from SOURCE"
    )

  let thresholdPercent = 0.05
      threshold'       = threshold thresholdPercent

  g        <- getStdGen
  outImage <- collage (PixelRGB 0 0 0)
                      (imagePaths args)
                      (threshold' (width args), threshold' (height args))
                      (width args             , height args)
                      g
  writeImage (outputPath args) outImage
 where
  argsParser :: Parser Args
  argsParser =
    Args
      <$> some
            (argument
              str
              (metavar "SOURCE..." <> help "Images available for collage")
            )
      -- As of 2020-08-28,
      -- 'optparse-applicative' does not support `some argument`
      -- followed by `argument`:
      -- <https://github.com/pcapriotti/optparse-applicative/issues/163>.
      <*> option
            str
            (short 't' <> long "target" <> metavar "DEST" <> help
              "Destination of collage image"
            )
      <*> option
            positiveInt
            (short 'w' <> long "width" <> metavar "WIDTH" <> help
              "Width of collage image"
            )
      <*> option
            positiveInt
            (short 'h' <> long "height" <> metavar "HEIGHT" <> help
              "Height of collage image"
            )
   where
    positiveInt :: ReadM Int
    positiveInt = eitherReader $ \arg -> case reads arg of
      [(r, "")] -> if r > 0
        then return r
        else Left $ "value `" ++ arg ++ "' must be positive"
      _ -> Left $ "value `" ++ arg ++ "' must be positive integer"

  threshold :: RealFrac a => Integral b => a -> b -> b
  threshold tp x = ceiling $ tp * fromIntegral x
