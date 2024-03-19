toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0    = []
  | otherwise = reverseList (toDigitsRev n) []
  where
    reverseList :: [Integer] -> [Integer] -> [Integer]
    reverseList [] reversedDigits = reversedDigits
    reverseList (x:xs) reversedDigits = reverseList xs (x : reversedDigits)

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0    = []
  | otherwise = let r = n `mod` 10
                    q = n `div` 10
                in r : toDigitsRev q

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:rs) = x : (y * 2) : doubleEveryOther rs

sumDigits :: [Integer] -> Integer
sumDigits xs = sum (concat (map toDigits xs))

validate :: Integer -> Bool
validate = ((\x -> if x `mod` 10 == 0 then True else False) . sumDigits . doubleEveryOther . toDigitsRev)

main :: IO ()
main = do
    putStrLn $ show $ validate 4012888888881881
    putStrLn $ show $ validate 4012888888881882