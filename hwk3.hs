import Data.Time
range from to count = next from count 
   where step = (to - from) / count
         next from count  
           | count <= 0 = []
           | otherwise  = from : next (from+step) (count-1)

absolute [] = []
absolute ((r,i) : rest) = sqrt(r*r + i*i) : absolute(rest)

rd _ [] = []
rd n (a:b) = f a : rd n b
  where f x = fromIntegral (round (c * x)) / c
        c = 10 ^ n

dft x =
  let n = fromIntegral $ length x
      index = range 0 n n
      xn = x `zip` index

      f [] = []
      f (k:rest) = (sum r, sum i) : f rest
        where (r, i) = unzip $ factor xn
              factor [] = []
              factor ((xj, j) : rest) = (xj * cos y, -xj * sin y) : factor rest
                where y = 2 * pi * j * k / n             
  in
      f index

split :: [a] -> ([a], [a])
split xs = foldr f ([], []) (zip xs [0..])
  where
    f (x, i) (evens, odds)
      | even i    = (x:evens, odds)
      | otherwise = (evens, x:odds)
      
fft :: [Double] -> [(Double, Double)]
fft xs | length xs < 17 = dft xs
       | otherwise = map fftStep $ zip xs [0..]
  where
    fftStep :: (Double, Int) -> (Double, Double)
    fftStep (x_val, idx) =
      let h = (length xs) `div` 2
          j@(fst_j, snd_j) = split xs
          index = idx `mod` h
          (a, b) = head $ drop index $ fft fst_j
          (c, d) = head $ drop index $ fft snd_j
          n = length xs
          u = cos ((-2) * pi * (fromIntegral index) / (fromIntegral n))
          v = sin ((-2) * pi * (fromIntegral index) / (fromIntegral n))
      in if fromIntegral idx < ((fromIntegral (length xs)) / 2.0)
         then (a + u * c - v * d, b + u * d + v * c)
         else (a - u * c + v * d, b - u * d - v * c)

main = do
        let n = 2^8
        let s1 = map (\x -> sin(20*pi*x) + sin(40*pi*x)/2) $ range 0 1 n
        -- print(rd 3 s1)
        start <- getCurrentTime
        let dft1 = map (\x -> x/n) $ absolute $ dft s1
        print(rd 2 dft1)
        end <- getCurrentTime
        print (diffUTCTime end start)
        start2 <- getCurrentTime
        let fft1 = map (\x -> x/n) $ absolute $ fft s1
        print(rd 2 fft1)
        end2 <- getCurrentTime
        print (diffUTCTime end2 start2)