module Lib
    ( generatePalette
    ) where

import Graphics.Gloss
    ( Picture(Pictures, Text),
      black,
      white,
      color,
      rectangleSolid,
      scale,
      translate,
      bitmapOfByteString,
      BitmapFormat(BitmapFormat),
      PixelFormat(PxRGBA),
      RowOrder(TopToBottom) )
import Graphics.Gloss.Export.PNG ( exportPictureToPNG )
import System.Environment (getArgs)
import System.Exit ( exitFailure )
import Data.Word (Word8)
import Data.ByteString (ByteString, pack, length)
import Data.Char(toUpper)
import Data.Monoid (All(getAll))

type RGBA = [Word8]

purple :: [Word8]
purple = [128, 0, 128, 255]

bitmapData :: ByteString
bitmapData = pack $ take 40000 (cycle purple)

-- https://stackoverflow.com/questions/14929437/haskell-recursively-convert-hex-string-to-integer/14929509
hexChar :: Char -> Int
hexChar ch
    | ch == '0' = 0
    | ch == '1' = 1
    | ch == '2' = 2
    | ch == '3' = 3
    | ch == '4' = 4
    | ch == '5' = 5
    | ch == '6' = 6
    | ch == '7' = 7
    | ch == '8' = 8
    | ch == '9' = 9
    | ch == 'A' = 10
    | ch == 'B' = 11
    | ch == 'C' = 12
    | ch == 'D' = 13
    | ch == 'E' = 14
    | ch == 'F' = 15
    | otherwise     = 0


parseHex :: String -> Int
parseHex [] = 0
parseHex hxStr = hexChar(last hxStr)+(16*parseHex(init hxStr))


sliceHex :: Int -> Int -> String -> Int
sliceHex from to xs = parseHex (take (to - from + 1) (drop from xs))


ourPicture :: Int -> Int -> ByteString -> Picture
ourPicture hsize vsize x = bitmapOfByteString hsize vsize (BitmapFormat TopToBottom PxRGBA) x True


getColorsHex :: String -> [Int]
getColorsHex c
    | head c == '#' = [sliceHex 1 2 c, sliceHex 3 4 c, sliceHex 5 6 c, 255]
    | otherwise = [sliceHex 0 1 c, sliceHex 2 3 c, sliceHex 4 5 c, 255]


getAllColorsHex :: [String] -> [[Int]]
getAllColorsHex = map getColorsHex


generateOneColor color size = take size (cycle color)


generateRow :: [[Int]] -> Int -> [Int]
generateRow [] _ = []
generateRow (c:rest) csize = generateOneColor c csize ++ generateRow rest csize


generateBitmapData :: [[Int]] -> Int -> Int -> Int -> [Int]
generateBitmapData colors colorHSize vsize i
    | i < vsize = generateRow colors colorHSize ++ generateBitmapData colors colorHSize vsize (i+1)
    | otherwise = []


addText :: [String] -> Float -> Float -> Float -> [Picture]
addText [] _ _ _ = []
addText (x:xs) offset offsetTo iterator
    | offset <= offsetTo =
        translate offset (-98) (scale 0.2 0.2  (color black (Text x))) : addText xs (offset + iterator) offsetTo iterator
    | otherwise = []


generatePalette :: IO ()
generatePalette = do
    args <- getArgs
    if head args /= "-e" && Prelude.length args >= 2
        then do
            Prelude.putStrLn "Usage: halette -e filepath_to_png [hex_colors]\nExaple: \n\tcommand: halette -e palette.png $(cat colors.txt)\n\tcolors.txt: #FFFFFF #AAAAAA #BBBBBB"
            exitFailure
        else do
            let path = args !! 1
            let numberOfColors = Prelude.length args - 2
            let hexUpper = map (map toUpper) (drop 2 args)
            let colors = getAllColorsHex hexUpper
            -- single pixel has 4 bytes, we need 160000 bytes to generate 200x200 image
            let bytes = pack $ map fromIntegral (generateBitmapData colors (4*200) 200 0)
            let rectSize = fromIntegral (200 * numberOfColors) :: Float
            let offsetTo = fromIntegral (200 * numberOfColors `div` 2) :: Float
            let increment = 200
            let pic = Pictures ([
                        ourPicture (200 * numberOfColors) 200 bytes,
                        translate 0 (-100) $ color white $ rectangleSolid rectSize 60
                        ] ++ addText hexUpper (-offsetTo) offsetTo increment)
            exportPictureToPNG (200 * numberOfColors, 200) white path pic
