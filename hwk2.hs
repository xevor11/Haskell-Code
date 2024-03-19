range :: Double -> Double -> Double -> [Double]
range x y z
  | z <= 0 = []
  | y <= x = []
  -- only one element in the range.
  | z == 0 = [x]
  | otherwise = x : range (x + step) y z
  where
    step = if z == 0 then (y - x) else (y / z)
    
rd :: Int -> [Double] -> [Double]
rd _ [] = []
rd n (x:xs) = (fromIntegral (round (x * 10^n))) / 10^n : rd n xs
    
absolute :: [(Double, Double)] -> [Double]
absolute = map (\(a, b) -> sqrt (a^2 + b^2))

dft :: [Double] -> [(Double, Double)]
dft xs = dft_helper xs 0

dft_helper :: [Double] -> Int -> [(Double, Double)]
dft_helper xs n
  | validIndex = [(realPart n, imagPart n)] ++ dft_helper xs (n + 1)
  | otherwise = []
  where
    validIndex = n < length xs
    realPart m = sum [x * cos (2 * pi * fromIntegral m * fromIntegral i / fromIntegral (length xs)) | (x, i) <- zip xs [0..]]
    imagPart m = - sum [x * sin (2 * pi * fromIntegral m * fromIntegral i / fromIntegral (length xs)) | (x, i) <- zip xs [0..]]

main = do
  let n = 64
  let s = map (\t -> sin(10*2*pi*t) + sin(20*2*pi*t)/2) $ range 0 1 n
  let result = map (\x -> x / n) $ absolute $ dft s
  print(rd 3 s)
  print(rd 2 result)
