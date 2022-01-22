module Generator where
import Graphics.Gloss (thickArc)

type RGB = [Float]

getRGB :: Int -> Int -> Int -> RGB
getRGB r g b = [fromIntegral r/255, fromIntegral g/255, fromIntegral b/255]

getIntFromRGB :: RGB -> [Int]
getIntFromRGB = map (round.(255*))

luminosity :: RGB -> Float
luminosity c = 0.5 * (maximum c + minimum c)


hue :: RGB -> Float
hue c
    | max == c !! 0 = (c !! 1 - c !! 2)/(max - min) * 60
    | max == c !! 1 = (2.0 + (c !! 2 - c !! 0)/(max-min)) * 60
    | otherwise = (4.0 + (c !! 0 - c !! 1) / (max-min)) * 60
    where
        max = maximum c
        min = minimum c


saturation :: RGB -> Float
saturation c
    | lum == 1 = 0
    | otherwise = (maximum c - minimum c) / (1 -  abs (2 * lum - 1))
    where
        lum = luminosity c


convertHSLtoRGB :: Float -> Float -> Float -> RGB
convertHSLtoRGB h s l
    | s == 0 = [l, l, l]
    | otherwise = [
                hueToRGB p q (h + 1/3),
                hueToRGB p q h,
                hueToRGB p q (h - 1/3)
            ]
    where
        q = if l < 0.5 then l * (1 + s) else l + s - l * s
        p = 2 * l - q
        hueToRGB p q t
            | nt < 1/6 = p + (q - p) * 6 * nt
            | nt < 1/2 = q
            | nt < 2/3 = p + (q - p) * (2/3 - nt) * 6
            | otherwise = p
            where
                nt
                  | t < 0 = t + 1
                  | t > 1 = t - 1
                  | otherwise = t


getAnalogicColor c variation = newC
    where
        newC = convertHSLtoRGB (calcVar/360) (saturation c) (luminosity c)
        calcVar
            | hue c + variation > 360 = (hue c + variation) - 360
            | hue c + variation < 0 = (hue c + variation) + 360
            | otherwise = hue c + variation


analogic :: RGB -> Float -> [RGB]
analogic c variation = [c, secondC, thridC]
    where
        secondC = convertHSLtoRGB (plus/360) (saturation c) (luminosity c)
        thridC = convertHSLtoRGB (minus/360) (saturation c) (luminosity c)
        plus = if hue c + variation > 360 then (hue c + variation) - 360 else hue c + variation
        minus  = if hue c - variation < 0 then 360 + (hue c - variation) else hue c - variation


getScale :: (Float, Float, Float) -> [(Float, Float, Float)]
getScale (h,s,l) = [(h, s, nl) | nl <- [l+lowestVal, l+10+lowestVal..l+lowestVal+60]]
    where
        lowestVal = lowest l (-30)
        lowest l mVal
            | l - abs mVal < 0 = lowest l (mVal + 10)
            | otherwise = mVal

getScaleAnalogic :: RGB -> [[RGB]]
getScaleAnalogic c = [map (\(h,s,l) -> convertHSLtoRGB h s l) (getScale x) | x <- [og, secondC, thridC]]
    where
        variation = 30
        og = (hue c/360, saturation c, luminosity c)
        secondC = (plus/360, saturation c, luminosity c)
        thridC = (minus/360, saturation c, luminosity c)
        plus = if hue c + variation > 360 then (hue c + variation) - 360 else hue c + variation
        minus  = if hue c - variation < 0 then 360 + (hue c - variation) else hue c - variation
