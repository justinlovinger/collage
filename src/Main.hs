{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Data.Bifunctor                 ( first )
import           Data.Char                      ( digitToInt
                                                , isHexDigit
                                                )
import           Numeric                        ( showFFloat )
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
                                                , showDefault
                                                , showDefaultWith
                                                , some
                                                , str
                                                , value
                                                )

data Position = Center | UpLeft | DownRight

collage
  :: RandomGen g
  => Pixel RGB Double
  -> [FilePath]
  -> (Int, Int)
  -> (Int, Int)
  -> g
  -> IO (Image VS RGB Double)
collage fillPx imagePaths thresh = go where
  go size g
    | cmpTup (||) (<=) size thresh = pure
    $ fill' (max 1 $ fst size, max 1 $ snd size)
    | otherwise = do
      let ((ga, gb), gc) = first split $ split g

      -- Select a random image
      -- and fill the remaining space
      -- with a collage of random images.
      imageA <- (pure . fit size) =<< (readImageRGB VS $ choose imagePaths ga)

      case
          choose
            (  [UpLeft, DownRight]
            ++ if cmpTup (&&)
                         (cmpTup (&&) (>))
                         (remainingHalves size imageA)
                         (thresh, thresh)
                 then [Center]
                 else []
            )
            gb
        of
          Center -> do
            let (sizeB, sizeC) = remainingHalves size imageA
                (gca  , gcb  ) = split gc
            imageB <- go sizeB gca
            imageC <- go sizeC gcb
            pure $ superimpose (0, 0) imageB $ combine
              size
              (posCenter size imageA)
              imageA
              (posDownRight size imageC)
              imageC
          UpLeft -> do
            imageB <- go (remaining size imageA) gc
            pure $ combine size (0, 0) imageA (posDownRight size imageB) imageB
          DownRight -> do
            imageB <- go (remaining size imageA) gc
            pure $ combine size (posDownRight size imageA) imageA (0, 0) imageB

  cmpTup
    :: (Bool -> Bool -> Bool) -> (a -> a -> Bool) -> (a, a) -> (a, a) -> Bool
  cmpTup comb pred ta tb = pred (fst ta) (fst tb) `comb` pred (snd ta) (snd tb)

  -- If `image` takes width of canvas,
  -- a horizontal slice remains.
  -- Otherwise, a vertical slice remains.
  remaining (w, h) image = if w' == w then (w, h - h') else (w - w', h)
    where (h', w') = dims image
  -- Note: `posCenter` and `remainingHalves` are coordinated
  -- such that the first of `remainingHalves` is the space on the up left
  -- of the centered image
  -- and the second of `remainingHalves` is the space on the down right
  -- of the centered image.
  remainingHalves (w, h) image = if w' == w
    then
      let h'' = fromIntegral (h - h') / 2 in ((w, ceiling h''), (w, floor h''))
    else
      let w'' = fromIntegral (w - w') / 2 in ((ceiling w'', h), (floor w'', h))
    where (h', w') = dims image

  posCenter size image = (f w, f h)   where
    f x = ceiling $ fromIntegral x / 2
    (w, h) = posDownRight size image
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
  , fillColor :: String
  , thresholdPercent :: Double
  }

main :: IO ()
main = do
  args <- execParser $ info
    (argsParser <**> helper)
    (fullDesc <> header "collage - create an image collage" <> progDesc
      "Create a collage of images randomly chosen from SOURCE"
    )
  let threshold' = threshold (thresholdPercent args)

  g        <- getStdGen
  outImage <- collage (readHexColor $ fillColor args)
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
      <*> option
            hexColor
            (  short 'c'
            <> long "fill-color"
            <> metavar "HEXCODE"
            <> help "Color to fill blank space in collage"
            <> showDefault
            <> value "#000000"
            )
      <*> option
            doubleIn01
            (  long "threshold"
            <> metavar "PERCENT"
            <> help "Percent of blank space allowed in collage"
            <> showDefaultWith (\x -> showFFloat Nothing x "")
            <> value 0.05
            )
   where
    doubleIn01 :: ReadM Double
    doubleIn01 = eitherReader $ \arg -> case reads arg of
      [(x, "")] -> if x > 0 && x < 1
        then return x
        else Left $ "value `" ++ arg ++ "' must be between 0 and 1, exclusive"
      _ ->
        Left $ "value `" ++ arg ++ "' must be number between 0 and 1, exclusive"

    hexColor :: ReadM String
    hexColor = eitherReader $ \arg ->
      if length arg == 7 && head arg == '#' && all isHexDigit (tail arg)
        then return arg
        else Left $ "value `" ++ arg ++ "' must be hex color code"

    positiveInt :: ReadM Int
    positiveInt = eitherReader $ \arg -> case reads arg of
      [(x, "")] -> if x > 0
        then return x
        else Left $ "value `" ++ arg ++ "' must be positive"
      _ -> Left $ "value `" ++ arg ++ "' must be positive integer"

  readHexColor :: String -> Pixel RGB Double
  readHexColor s = PixelRGB (fromHexPair $ slice 1 2 s)
                            (fromHexPair $ slice 3 4 s)
                            (fromHexPair $ slice 5 6 s)
   where
    fromHexPair :: [Char] -> Double
    fromHexPair (h1 : h2 : _) =
      (fromIntegral $ 16 * digitToInt h1 + digitToInt h2) / 255

    slice :: Int -> Int -> [a] -> [a]
    slice a b = take (1 + b - a) . drop a

  threshold :: RealFrac a => Integral b => a -> b -> b
  threshold tp x = ceiling $ tp * fromIntegral x
