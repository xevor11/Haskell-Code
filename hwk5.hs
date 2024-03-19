import Data.Complex
newtype Vec a = Vec {runVec :: [a]}

instance Show a => Show (Vec a) where
    show (Vec xs) = "[" ++ showList xs ++ "]"
        where
            showList [] = ""
            showList [x] = show x
            showList (x:xs) = show x ++ " " ++ showList xs

instance Num a => Num (Vec a) where
    (+) (Vec []) scalar = scalar
    (+) scalar (Vec []) = scalar
    (Vec xs) + (Vec ys) = Vec [x + y | (x, y) <- zip xs ys]
    
    (-) (Vec []) scalar = scalar
    (-) scalar (Vec []) = scalar
    (Vec xs) - (Vec ys) = Vec [x - y | (x, y) <- zip xs ys]
    
    (*) (Vec []) scalar = scalar
    (*) scalar (Vec []) = scalar
    (Vec xs) * (Vec ys) = Vec [x * y | (x, y) <- zip xs ys]
    
    negate (Vec xs) = Vec (map negate xs)
    abs (Vec xs) = Vec (map abs xs)
    signum (Vec xs) = Vec (map signum xs)
    fromInteger x = Vec (repeat (fromInteger x))

instance (Floating a) => Fractional (Vec a) where
    (Vec []) / (Vec []) = error "Division by zero."
    (Vec xs) / (Vec ys) = Vec [x / y | (x, y) <- zip xs ys]
    fromRational x = Vec (repeat (fromRational x))


instance Floating a => Floating (Vec a) where
    pi = Vec (repeat pi)
    exp (Vec xs) = Vec (map exp xs)
    log (Vec xs) = Vec (map log xs)
    sin (Vec xs) = Vec (map sin xs)
    cos (Vec xs) = Vec (map cos xs)
    asin (Vec xs) = Vec (map asin xs)
    acos (Vec xs) = Vec (map acos xs)
    atan (Vec xs) = Vec (map atan xs)
    sinh (Vec xs) = Vec (map sinh xs)
    cosh (Vec xs) = Vec (map cosh xs)
    asinh (Vec xs) = Vec (map asinh xs)
    acosh (Vec xs) = Vec (map acosh xs)
    atanh (Vec xs) = Vec (map atanh xs)

instance Foldable Vec where
    foldr _ c (Vec []) = c
    foldr f c (Vec (a:b)) = f a $ foldr f c (Vec b)

pure' x = Vec (repeat x)

realV :: Num a => Vec a -> Vec (Complex a)
-- realV (Vec []) = Vec []
-- realV (Vec (x:xs)) = Vec ((x :+ 0) : rest)
--     where Vec rest = realV (Vec xs)
realV (Vec xs) = Vec (map (:+ 0) xs)

imagV :: Num a => Vec a -> Vec (Complex a)
-- imagV (Vec []) = Vec []
-- imagV (Vec (y:ys)) = Vec ((0 :+ y) : rest)
--     where Vec rest = imagV (Vec ys)
imagV (Vec xs) = Vec (map (0 :+) xs)
    
main = do
    let v1 = Vec [1,2,3]
    let v2 = Vec [2,3,4]
    let v3 = Vec [-10,0,10]
    print $ v1 + v2
    print $ v1 - v2
    print $ v1 * v2
    print $ v1 / v2
    print $ negate v1
    print $ signum v3
    print $ abs v3
    print $ v1 + 10
    print $ v2 + 1.2
    print $ v1 + (pure' $ sqrt 2)
    print $ realV v1
    print $ imagV v1
    print $ realV v1 + imagV v2
    print $ sin $ v1 * (pi / 2)
    print $ sum v1
